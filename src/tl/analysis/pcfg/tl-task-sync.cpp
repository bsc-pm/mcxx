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

#include "cxx-cexpr.h"
#include "tl-datareference.hpp"
#include "tl-task-sync.hpp"
#include "tl-tribool.hpp"

#include <queue>

namespace TL {
namespace Analysis {
namespace TaskAnalysis{

    // **************************************************************************************************** //
    // *************************** Class implementing task PCFG synchronization *************************** //

namespace {
    // #define TASK_SYNC_DEBUG
    bool function_waits_tasks(TL::Symbol sym)
    {
        scope_entry_t* entry = sym.get_internal_symbol();

        int i;
        for (i = 0; i < entry->entity_specs.num_gcc_attributes; i++)
        {
            const char* attr_name = entry->entity_specs.gcc_attributes[i].attribute_name;
            if (attr_name != NULL
                    && std::string(attr_name) == "omp_waits_tasks")
            {
                return true;
            }
        }

        return false;
    }

    void set_sync_relationship(tribool& task_sync_rel,
            AliveTaskSet::iterator& alive_tasks_it,
            PointsOfSync& points_of_sync,
            Node* current,
            Node* current_sync_point)
    {
        switch (task_sync_rel.value())
        {
            case tribool::unknown :
            case tribool::yes :
                {
                    SyncKind sync_kind = Sync_maybe;
                    if (task_sync_rel == tribool::yes)
                        sync_kind = Sync_static;

                    if (points_of_sync.find(alive_tasks_it->node) != points_of_sync.end())
                    {
                        points_of_sync[alive_tasks_it->node].insert(std::make_pair(current_sync_point, sync_kind));
#ifdef TASK_SYNC_DEBUG
                        std::cerr << __FILE__ << ":" << __LINE__
                            << " task (among others) maybe synchronizes in this task execution" << std::endl;
#endif
                    }
                    else
                    {
                        points_of_sync[alive_tasks_it->node].insert(std::make_pair(current_sync_point, sync_kind));
#ifdef TASK_SYNC_DEBUG
                        std::cerr << __FILE__ << ":" << __LINE__
                            << " task maybe synchronizes in this task execution" << std::endl;
#endif
                    }

#ifdef TASK_SYNC_DEBUG
                    std::pair<AliveTaskSet::iterator, bool> res =
#endif
                    // TODO - We do this even if task_sync_rel returned tribool::yes. Why?
                    current->get_live_out_tasks().insert(*alive_tasks_it);
#ifdef TASK_SYNC_DEBUG
                    if (res.second)
                    {
                            std::cerr << __FILE__ << ":" << __LINE__
                            << " task is (potentially) still alive after execution" << std::endl;
                    }
#endif

                    if (task_sync_rel == tribool::yes)
                    {
#ifdef TASK_SYNC_DEBUG
                        std::cerr << __FILE__ << ":" << __LINE__ << " but we know it statically synchronizes" << std::endl;
#endif
                        current->get_static_sync_out_tasks().insert(*alive_tasks_it);
                    }
                    break;
                }
            case tribool::no :
                {
                    // We positively know that the task does not synchronize here
#ifdef TASK_SYNC_DEBUG
                    std::pair<AliveTaskSet::iterator, bool> res =
#endif
                    current->get_live_out_tasks().insert(*alive_tasks_it);
#ifdef TASK_SYNC_DEBUG
                    if (res.second)
                    {
                        std::cerr << __FILE__ << ":" << __LINE__
                            << " task is (for sure) still alive after execution" << std::endl;
                    }
#endif
                    break;
                }
            default:
                {
                    internal_error("Code unreachable", 0);
                }
        }

    }

    bool has_task_creation_edges(Node* n)
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

    std::string print_set(AliveTaskSet& t)
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

    tribool may_have_dependence(Nodecl::NodeclBase source, Nodecl::NodeclBase target)
    {
        TL::DataReference source_data_ref(source);
        TL::DataReference target_data_ref(target);

        // We return unknown
        if (!source_data_ref.is_valid()
                || !target_data_ref.is_valid())
            return tribool();

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
            return (source_sym == target_sym) ? tribool::yes : tribool::no;
        }

        bool source_is_subobject = is_strict_subobject_access(source_data_ref);
        bool target_is_subobject = is_strict_subobject_access(target_data_ref);

        if ((source_is_object || source_is_subobject)
                && (target_is_object || target_is_subobject))
        {
            // If one is object and the other subobject (or both subobjects),
            // if the base symbol is different, they they cannot be the same dependence
            //
            // TODO - Sometimes the two subobjects may be the same and we
            // TODO - may want to return tribool::yes
            if (source_sym != target_sym)
                return tribool::no;
        }

        // In all other cases we take a conservative stance
        return tribool::unknown;
    }

    tribool may_have_dependence_list(Nodecl::List out_deps_source, Nodecl::List in_deps_target)
    {
        tribool result = tribool::no;
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

    bool is_only_input_dependence(Nodecl::NodeclBase n)
    {
        return n.is<Nodecl::OpenMP::DepIn>()
            || n.is<Nodecl::OpenMP::DepInAlloca>();
    }

    tribool compute_taskwait_sync_relationship(Node* source, Node* target)
    {
        // Source (task)
        Nodecl::NodeclBase task_node_source = source->get_graph_related_ast();
        ERROR_CONDITION(task_node_source.is_null(), "Invalid source task tree", 0);
        ERROR_CONDITION(!task_node_source.is<Nodecl::OpenMP::Task>()
                && !task_node_source.is<Nodecl::OpenMP::TaskExpression>()
                && !task_node_source.is<Nodecl::OpenMP::TaskCall>(),
                "Expecting an OpenMP::Task, OpenMP::TaskExpression or OpenMP::TaskCall source node here got a %s",
                ast_print_node_type(task_node_source.get_kind()));
        if (task_node_source.is<Nodecl::OpenMP::TaskExpression>())
        {
            // Return unknown
            return tribool();
        }

        Nodecl::List task_source_env;
        if (task_node_source.is<Nodecl::OpenMP::Task>())
        {
            Nodecl::OpenMP::Task task_source(task_node_source.as<Nodecl::OpenMP::Task>());
            task_source_env = task_source.get_environment().as<Nodecl::List>();
        }
        else if (task_node_source.is<Nodecl::OpenMP::TaskCall>())
        {
            Nodecl::OpenMP::TaskCall task_source(task_node_source.as<Nodecl::OpenMP::TaskCall>());
            task_source_env = task_source.get_environment().as<Nodecl::List>();
        }
        else
        {
            internal_error("Code unreachable", 0);
        }

        // TODO - Concurrent and commutative are not handled yet
        // TODO - OpenMP::Base may create more than ONE dep_xxx tree (here we would overwrite them)
        Nodecl::NodeclBase source_dep_in;
        Nodecl::NodeclBase source_dep_out;
        Nodecl::NodeclBase source_dep_inout;
        Nodecl::NodeclBase source_dep_in_alloca;
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
            else if (it->is<Nodecl::OpenMP::DepInAlloca>())
                source_dep_in_alloca = *it;
        }

        // Target (taskwait)
        ERROR_CONDITION(target->get_type() != __OmpWaitonDeps, "Invalid node", 0);
        TL::ObjectList<Nodecl::NodeclBase> task_node_target_stmts = target->get_statements();
        ERROR_CONDITION(task_node_target_stmts.size() != 1, "Invalid list of statements", 0);
        Nodecl::NodeclBase taskwait_node_target = task_node_target_stmts[0];
        ERROR_CONDITION(taskwait_node_target.is_null(), "Invalid target task tree", 0);
        ERROR_CONDITION(!taskwait_node_target.is<Nodecl::OpenMP::WaitOnDependences>(),
                "Expecting an OpenMP::WaitOnDependences target node here got a %s",
                ast_print_node_type(taskwait_node_target.get_kind()));

        Nodecl::List task_target_env = taskwait_node_target
            .as<Nodecl::OpenMP::WaitOnDependences>()
            .get_environment()
            .as<Nodecl::List>();

        Nodecl::NodeclBase target_dep_in;
        Nodecl::NodeclBase target_dep_out;
        Nodecl::NodeclBase target_dep_inout;
        for (Nodecl::List::iterator it = task_target_env.begin();
                it != task_target_env.end();
                it++)
        {
            ERROR_CONDITION((it->is<Nodecl::OpenMP::DepIn>()) || (it->is<Nodecl::OpenMP::DepOut>()),
                    "Unexpected tree in a taskwait", 0);
            if (it->is<Nodecl::OpenMP::DepInout>())
                target_dep_inout = *it;
        }

        tribool may_have_dep = tribool::no;

        // DRY
        Nodecl::NodeclBase sources[] = { source_dep_in, source_dep_in_alloca, source_dep_inout, source_dep_out };
        int num_sources = sizeof(sources)/sizeof(sources[0]);
        Nodecl::NodeclBase targets[] = { target_dep_in, source_dep_in_alloca, target_dep_inout, target_dep_out };
        int num_targets = sizeof(targets)/sizeof(targets[0]);

        for (int n_source = 0; n_source < num_sources; n_source++)
        {
            for (int n_target = 0; n_target < num_targets; n_target++)
            {
                if (sources[n_source].is_null()
                        || targets[n_target].is_null())
                    continue;

                may_have_dep = may_have_dep || 
                    // At least one of the dependences is not only an input
                    ((!is_only_input_dependence(sources[n_source])
                      || !is_only_input_dependence(targets[n_target]))
                     // Note we (ab)use the fact that DepIn/DepOut/DepInOut/DepInAlloca all have the
                     // same physical layout
                     && may_have_dependence_list(
                         sources[n_source].as<Nodecl::OpenMP::DepOut>().get_out_deps().as<Nodecl::List>(),
                         targets[n_target].as<Nodecl::OpenMP::DepIn>().get_in_deps().as<Nodecl::List>()));
            }
        }

        return may_have_dep;
    }

    // Computes if task source will synchronize with the creation of the task target
    tribool compute_task_sync_relationship(Node* source, Node* target)
    {
#ifdef TASK_SYNC_DEBUG
        std::cerr << "CHECKING DEPENDENCES STATICALLY " << source << " -> " << target << std::endl;
#endif

        // TL::ObjectList<Nodecl::NodeclBase> source_statements = source->get_statements();
        // ERROR_CONDITION(source_statements.empty(), "Invalid source statement set", 0);
        Nodecl::NodeclBase task_node_source = source->get_graph_related_ast();
        ERROR_CONDITION(task_node_source.is_null(), "Invalid source task tree", 0);
        ERROR_CONDITION(!task_node_source.is<Nodecl::OpenMP::Task>()
                && !task_node_source.is<Nodecl::OpenMP::TaskExpression>()
                && !task_node_source.is<Nodecl::OpenMP::TaskCall>(),
                "Expecting an OpenMP::Task, OpenMP::TaskExpression or OpenMP::TaskCall source node here got a %s",
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
        else if (task_node_source.is<Nodecl::OpenMP::TaskExpression>())
        {
            // Return unknown
            return tribool();
        }
        else
        {
            internal_error("Code unreachable", 0);
        }

        Nodecl::NodeclBase source_dep_in;
        Nodecl::NodeclBase source_dep_in_alloca;
        Nodecl::NodeclBase source_dep_out;
        Nodecl::NodeclBase source_dep_inout;
        for (Nodecl::List::iterator it = task_source_env.begin();
                it != task_source_env.end();
                it++)
        {
            if (it->is<Nodecl::OpenMP::DepIn>())
                source_dep_in = *it;
            else if (it->is<Nodecl::OpenMP::DepInAlloca>())
                source_dep_in_alloca = *it;
            else if (it->is<Nodecl::OpenMP::DepOut>())
                source_dep_out = *it;
            else if (it->is<Nodecl::OpenMP::DepInout>())
                source_dep_inout = *it;
        }

        // TL::ObjectList<Nodecl::NodeclBase> target_statements = target->get_statements();
        // ERROR_CONDITION(target_statements.empty(), "Invalid target statement set", 0);
        Nodecl::NodeclBase task_node_target = target->get_graph_related_ast();
        ERROR_CONDITION(task_node_source.is_null(), "Invalid target task tree", 0);
        ERROR_CONDITION(!task_node_target.is<Nodecl::OpenMP::Task>()
                && !task_node_target.is<Nodecl::OpenMP::TaskExpression>()
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
        else if (task_node_target.is<Nodecl::OpenMP::TaskExpression>())
        {
            // Return unknown
            return tribool();
        }
        else
        {
            internal_error("Code unreachable", 0);
        }

        Nodecl::NodeclBase target_dep_in;
        Nodecl::NodeclBase target_dep_in_alloca;
        Nodecl::NodeclBase target_dep_out;
        Nodecl::NodeclBase target_dep_inout;
        for (Nodecl::List::iterator it = task_target_env.begin();
                it != task_target_env.end();
                it++)
        {
            if (it->is<Nodecl::OpenMP::DepIn>())
                target_dep_in = *it;
            else if (it->is<Nodecl::OpenMP::DepInAlloca>())
                target_dep_in_alloca = *it;
            else if (it->is<Nodecl::OpenMP::DepOut>())
                target_dep_out = *it;
            else if (it->is<Nodecl::OpenMP::DepInout>())
                target_dep_inout = *it;
        }

        tribool may_have_dep = tribool::no;

        // DRY
        Nodecl::NodeclBase sources[] = { source_dep_in, source_dep_in_alloca, source_dep_inout, source_dep_out };
        int num_sources = sizeof(sources)/sizeof(sources[0]);
        Nodecl::NodeclBase targets[] = { target_dep_in, source_dep_in_alloca, target_dep_inout, target_dep_out };
        int num_targets = sizeof(targets)/sizeof(targets[0]);

        for (int n_source = 0; n_source < num_sources; n_source++)
        {
            for (int n_target = 0; n_target < num_targets; n_target++)
            {
                if (sources[n_source].is_null()
                        || targets[n_target].is_null())
                    continue;

                may_have_dep = may_have_dep || 
                    // At least one of the dependences is not only an input
                    ((!is_only_input_dependence(sources[n_source])
                      || !is_only_input_dependence(targets[n_target]))
                     // Note we (ab)use the fact that DepIn/DepOut/DepInOut/DepInAlloca all have the
                     // same physical layout
                     && may_have_dependence_list(
                         sources[n_source].as<Nodecl::OpenMP::DepOut>().get_out_deps().as<Nodecl::List>(),
                         targets[n_target].as<Nodecl::OpenMP::DepIn>().get_in_deps().as<Nodecl::List>()));
            }
        }

        return may_have_dep;
    }

    void shallow_synchronization_point(
            Node* current,
            int current_domain_id,
            PointsOfSync& points_of_sync)
    {
        // This is a taskwait without dependences
        for (AliveTaskSet::iterator alive_tasks_it = current->get_live_in_tasks().begin();
                alive_tasks_it != current->get_live_in_tasks().end();
                alive_tasks_it++)
        {
            if (alive_tasks_it->domain != current_domain_id)
                continue;

            // Check if this (*alive_task_it) has already some point of synchronization
            if (points_of_sync.find(alive_tasks_it->node) != points_of_sync.end())
            {
                // Yes, it DOES, have some point of synchronization
                // Check now if (*alive_tasks_it) is NOT in the set of static synchronized tasks
                if (current->get_static_sync_in_tasks().find(*alive_tasks_it) == current->get_static_sync_in_tasks().end())
                {
                    // It is NOT in the set of static synchronized task so
                    // define a strict synchronization here
                    points_of_sync[alive_tasks_it->node].insert(std::make_pair(current, Sync_strict));
#ifdef TASK_SYNC_DEBUG
                    std::cerr << __FILE__ << ":" << __LINE__
                        << " Task synchronizes in this taskwait (among others) of domain " << current_domain_id << std::endl;
#endif
                }
                else
                {
                    // Well, (*alive_tasks_it) IS in the set of static
                    // synchronized tasks so it won't synchronize here
                    points_of_sync[alive_tasks_it->node].erase(std::make_pair(current, Sync_strict));
                }
            }
            else
            {
                // (*alive_tasks_it) DOES NOT have any synchronization point
                // Check now if (*alive_tasks_it) is NOT in the set of static synchronized tasks
                //
                // FIXME - Is this check ever false?
                if (current->get_static_sync_in_tasks().find(*alive_tasks_it) == current->get_static_sync_in_tasks().end())
                {
                    points_of_sync[alive_tasks_it->node].insert(std::make_pair(current, Sync_strict));
#ifdef TASK_SYNC_DEBUG
                    std::cerr << __FILE__ << ":" << __LINE__
                        << " Task synchronizes in this taskwait of domain " << current_domain_id << std::endl;
#endif
                }
            }
        }

        // Remove all the tasks that do sync here
        AliveTaskSet still_alive;
        for (AliveTaskSet::iterator alive_tasks_it = current->get_live_in_tasks().begin();
                alive_tasks_it != current->get_live_in_tasks().end();
                alive_tasks_it++)
        {
            if (alive_tasks_it->domain != current_domain_id)
                still_alive.insert(*alive_tasks_it);
        }
        current->get_live_out_tasks() = still_alive;
    }

    void compute_task_synchronizations_rec(Node* current,
            bool &changed,
            PointsOfSync& points_of_sync,
            int current_domain_id,
            int &next_domain_id)
    {
        if (current->is_visited())
            return;

        current->set_visited(true);

        // Propagate predecessors
        if (!current->is_entry_node())
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
                AliveTaskSet& alive_tasks_of_predecessor = predecessor->get_live_out_tasks();

                current->get_live_in_tasks().insert(alive_tasks_of_predecessor.begin(), alive_tasks_of_predecessor.end());
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
                    intersection = predecessor->get_static_sync_out_tasks();
#ifdef TASK_SYNC_DEBUG
                    std::cerr << "FIRST ISECT -> " << print_set(intersection) << std::endl;
#endif
                    first = false;
                }
                else
                {
                    StaticSyncTaskSet tmp;
#ifdef TASK_SYNC_DEBUG
                    std::cerr << "CURRENT ISECT -> " << print_set(intersection) << std::endl;
                    std::cerr << "OPERAND ISECT -> " << print_set(predecessor->get_static_sync_out_tasks()) << std::endl;
#endif
                    std::set_intersection(intersection.begin(),
                            intersection.end(),
                            predecessor->get_static_sync_out_tasks().begin(),
                            predecessor->get_static_sync_out_tasks().end(),
                            std::inserter(tmp, tmp.begin()));
                    intersection = tmp;
#ifdef TASK_SYNC_DEBUG
                    std::cerr << "COMPUTED ISECT -> " << print_set(intersection) << std::endl;
#endif
                }
            }
            current->get_static_sync_in_tasks() = intersection;
        }

        AliveTaskSet initial_alive_out = current->get_live_out_tasks();
        StaticSyncTaskSet initial_static_sync = current->get_static_sync_out_tasks();

#ifdef TASK_SYNC_DEBUG
        std::cerr << "["
                    << current->get_id()
                    << ":" << current->get_type_as_string()
                    << ":" << (current->is_graph_node() ? current->get_graph_type_as_string() : "")
                    << "]"
                    << "Before" << std::endl
                    << "  IN[alive] = " << print_set(current->get_live_in_tasks()) << std::endl
                    << "  OUT[alive] = " << print_set(current->get_live_out_tasks()) << std::endl
                    << "  IN[static_sync] = " << print_set(current->get_static_sync_in_tasks()) << std::endl
                    << "  OUT[static_sync] = " << print_set(current->get_static_sync_out_tasks()) << std::endl;
#endif

        if (current->is_omp_task_node())
        {
            int new_domain_id = next_domain_id++;
            compute_task_synchronizations_rec(current->get_graph_entry_node(), changed, points_of_sync, new_domain_id, next_domain_id);
        }
        else if (current->is_omp_barrier_graph_node())
        {
            // Synchronize with every task alive but not statically synchronized
            for (AliveTaskSet::iterator alive_tasks_it = current->get_live_in_tasks().begin();
                    alive_tasks_it != current->get_live_in_tasks().end();
                    alive_tasks_it++)
            {
                if (points_of_sync.find(alive_tasks_it->node) != points_of_sync.end())
                {
                    if (current->get_static_sync_in_tasks().find(*alive_tasks_it) == current->get_static_sync_in_tasks().end())
                    {
                        points_of_sync[alive_tasks_it->node].insert(std::make_pair(current, Sync_strict));
#ifdef TASK_SYNC_DEBUG
                        std::cerr << __FILE__ << ":" << __LINE__ << " Task synchronizes in this barrier (among others)" << std::endl;
#endif
                    }
                    else
                    {
                        points_of_sync[alive_tasks_it->node].erase(std::make_pair(current, Sync_strict));
                    }
                }
                else
                {
                    if (current->get_static_sync_in_tasks().find(*alive_tasks_it) == current->get_static_sync_in_tasks().end())
                    {
                        points_of_sync[alive_tasks_it->node].insert(std::make_pair(current, Sync_strict));
#ifdef TASK_SYNC_DEBUG
                        std::cerr << __FILE__ << ":" << __LINE__ << " Task synchronizes in this barrier" << std::endl;
#endif
                    }
                }
            }
            current->get_live_out_tasks().clear();
            current->get_static_sync_out_tasks().clear();
        }
        else if (current->is_graph_node())
        {
            Node* graph_entry = current->get_graph_entry_node();
            graph_entry->get_live_in_tasks() = current->get_live_in_tasks();
            graph_entry->get_static_sync_in_tasks() = current->get_static_sync_in_tasks();

            compute_task_synchronizations_rec(graph_entry, changed, points_of_sync, current_domain_id, next_domain_id);

            Node* graph_exit = current->get_graph_exit_node();
            current->get_live_out_tasks() = graph_exit->get_live_out_tasks();
            current->get_static_sync_out_tasks() = graph_exit->get_static_sync_out_tasks();
        }
        else if (current->is_omp_taskwait_node())
        {
            shallow_synchronization_point(current, current_domain_id, points_of_sync);
        }
        else if (current->is_ompss_taskwait_on_node())
        {
            for (AliveTaskSet::iterator alive_tasks_it = current->get_live_in_tasks().begin();
                    alive_tasks_it != current->get_live_in_tasks().end();
                    alive_tasks_it++)
            {
                tribool task_sync_rel = tribool::no;
                if (alive_tasks_it->domain == current_domain_id)
                {
                    task_sync_rel = compute_taskwait_sync_relationship(alive_tasks_it->node, current);
                }
                set_sync_relationship(task_sync_rel, alive_tasks_it, points_of_sync, current, current);
            }
        }
        else if (current->is_omp_task_creation_node())
        {
            ObjectList<Edge*> task_creation = get_task_creation_edges(current);
            ERROR_CONDITION(task_creation.size() != 1, "Too many creation edges", 0);
            Node* task = task_creation[0]->get_target();

            // Synchronize with existing tasks of the same domain
            for (AliveTaskSet::iterator alive_tasks_it = current->get_live_in_tasks().begin();
                    alive_tasks_it != current->get_live_in_tasks().end();
                    alive_tasks_it++)
            {
                tribool task_sync_rel = tribool::no;
                if (alive_tasks_it->domain == current_domain_id)
                {
                    task_sync_rel = compute_task_sync_relationship(alive_tasks_it->node, task);
                }
                set_sync_relationship(task_sync_rel, alive_tasks_it, points_of_sync, current, task);
            }

            // Add the newly created task as well
            std::pair<AliveTaskSet::iterator, bool> res = current->get_live_out_tasks().insert(AliveTaskItem(task, current_domain_id));
#ifdef TASK_SYNC_DEBUG
            if (res.second)
            {
                std::cerr << __FILE__ << ":" << __LINE__
                                << " new task alive in " << task->get_id() << " with domain " << current_domain_id << std::endl;
            }
#endif

            // All the alive tasks at the end of the task are also alive
            // after the task creation (if it is run immediately)
            if (task->is_graph_node())
            {
                Node* exit_of_task = task->get_graph_exit_node();
                for (AliveTaskSet::iterator alive_tasks_it = exit_of_task->get_live_in_tasks().begin();
                        alive_tasks_it != exit_of_task->get_live_in_tasks().end();
                        alive_tasks_it++)
                {
                    res = current->get_live_out_tasks().insert(*alive_tasks_it);
#ifdef TASK_SYNC_DEBUG
                    if (res.second)
                    {
                        std::cerr << __FILE__ << ":" << __LINE__
                            << " task created in task outlives its parent task" << std::endl;
                    }
#endif
                }
            }
        }
        else if (current->is_function_call_node())
        {
            TL:: Symbol symbol = current->get_function_node_symbol();

            if (symbol.is_valid())
            {
                // TODO - We do not have enough information if we lack the function code
                //
                if (function_waits_tasks(symbol))
                {
                    shallow_synchronization_point(current, current_domain_id, points_of_sync);
                }
                else
                {
                    // All other nodes just propagate OUT(X) = IN(X)
                    current->get_live_out_tasks() = current->get_live_in_tasks();
                    current->get_static_sync_out_tasks() = current->get_static_sync_in_tasks();
                }
            }
        }
        else
        {
            // All other nodes just propagate OUT(X) = IN(X)
            current->get_live_out_tasks() = current->get_live_in_tasks();
            current->get_static_sync_out_tasks() = current->get_static_sync_in_tasks();
        }

        if (initial_alive_out != current->get_live_out_tasks()
                || initial_static_sync != current->get_static_sync_out_tasks())
        {
#ifdef TASK_SYNC_DEBUG
            std::cerr << "[" << current->get_id() << "] OUT SET HAS CHANGED" << std::endl;
#endif
            changed = true;
        }

#ifdef TASK_SYNC_DEBUG
        std::cerr << "["
            << current->get_id()
            << ":" << current->get_type_as_string()
            << ":" << (current->is_graph_node() ? current->get_graph_type_as_string() : "")
            << "]"
            << "After" << std::endl
            << "  IN[alive] = " << print_set(current->get_live_in_tasks()) << std::endl
            << "  OUT[alive] = " << print_set(current->get_live_out_tasks()) << std::endl
            << "  IN[static_sync] = " << print_set(current->get_static_sync_in_tasks()) << std::endl
            << "  OUT[static_sync] = " << print_set(current->get_static_sync_out_tasks()) << std::endl;
#endif

        ObjectList<Edge*> exit_edges = current->get_exit_edges();
        for (ObjectList<Edge*>::iterator edge_it = exit_edges.begin();
                edge_it != exit_edges.end();
                edge_it++)
        {
            compute_task_synchronizations_rec((*edge_it)->get_target(), changed, points_of_sync, current_domain_id, next_domain_id);
        }
    }
    
    static bool IsOmpssEnabled = false;
}

    TaskSynchronizations::TaskSynchronizations(ExtensibleGraph* graph, bool is_ompss_enabled)
        : _graph(graph)
    {
        IsOmpssEnabled = is_ompss_enabled;
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
#ifdef TASK_SYNC_DEBUG
            std::cerr << std::endl << std::endl;
#endif
        } while (changes);

#ifdef TASK_SYNC_DEBUG
        std::cerr << "Task synchronizations computed" << std::endl;
#endif

        for (PointsOfSync::iterator it = points_of_sync.begin();
                it != points_of_sync.end();
                it++)
        {
            for (PointOfSyncSet::iterator jt = it->second.begin();
                    jt != it->second.end();
                    jt++)
            {
#ifdef TASK_SYNC_DEBUG
                std::cerr << "CONNECTING " << it->first->get_id() << " -> " << (*jt).first->get_id() << std::endl;
#endif
                Edge* edge = _graph->connect_nodes(it->first, (*jt).first, __Always,
                                                   Nodecl::NodeclBase::null(), /*is task edge*/ true);
                const char* s = sync_kind_to_str((*jt).second);
                edge->set_label(Nodecl::StringLiteral::make(Type(get_literal_string_type( strlen(s)+1, get_char_type() )),
                                                            const_value_make_string(s, strlen(s))));
            }
        }

        Node* post_sync = _graph->create_unconnected_node(__OmpVirtualTaskSync, Nodecl::NodeclBase::null());

        Node* exit = root->get_graph_exit_node();
        for (AliveTaskSet::iterator it = exit->get_live_in_tasks().begin();
                it != exit->get_live_in_tasks().end();
                it++)
        {
            if (exit->get_static_sync_in_tasks().find(*it) == exit->get_static_sync_in_tasks().end())
            {
#ifdef TASK_SYNC_DEBUG
                std::cerr << "CONNECTING VIRTUAL SYNC " << it->node->get_id() << " -> " << post_sync->get_id() << std::endl;
#endif
                Edge* edge = _graph->connect_nodes(it->node, post_sync, __Always,
                                                   Nodecl::NodeclBase::null(), /*is task edge*/ true);
                const char* s = sync_kind_to_str(Sync_post);
                edge->set_label(Nodecl::StringLiteral::make(Type(get_literal_string_type(strlen(s)+1, get_char_type())),
                                                            const_value_make_string(s, strlen(s))));
            }
        }
    }

    // ************************* END class implementing task PCFG synchronization ************************* //
    // **************************************************************************************************** //



    // **************************************************************************************************** //
    // *************************** Class implementing task concurrency analysis *************************** //

namespace {
    void collect_tasks_between_nodes( Node* current, Node* last, Node* skip, ObjectList<Node*>& result )
    {
        if( !current->is_visited( ) && ( current != last ) )
        {
            current->set_visited( true );

            if( current->is_exit_node( ) )
                return;

            if( current->is_graph_node( ) )
            {
                // Add inner tasks recursively, if exist
                collect_tasks_between_nodes( current->get_graph_entry_node( ), last, skip, result );

                // Add current node if it is a task
                if( current->is_omp_task_node( ) && ( current != skip ) )
                    result.insert( current );
            }

            ObjectList<Node*> children = current->get_children( );
            for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
            {
                collect_tasks_between_nodes( *it, last, skip, result );
            }
        }
    }

    bool sync_in_all_branches( Node* current, Node* original )
    {
        bool res = false;

        if( !current->is_visited_aux( ) )
        {
            current->set_visited_aux( true );

            if( !current->is_exit_node( ) )
            {
                if( current->is_graph_node( ) )
                {
                    if( current->is_ifelse_statement( ) || current->is_switch_statement( ) )
                    {
                        Node* condition = current->get_graph_entry_node( )->get_children( )[0];
                        ObjectList<Node*> children = condition->get_children( );
                        bool partial_res = true;
                        for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ) && partial_res; ++it )
                        {
                            partial_res = partial_res && sync_in_all_branches( *it, original );
                        }
                        res = partial_res;
                    }
                    else if( current->is_omp_barrier_graph_node( ) )
                    {
                        res = true;
                    }
                    else
                    {
                        res = sync_in_all_branches( current->get_graph_entry_node( ), original );
                    }
                }
                else if( current->is_omp_taskwait_node( ) )
                {
                    res = true;
                }

                // If we are navigating inside a graph node
                if( !res && ( current != original ) )
                {
                    ObjectList<Node*> children = current->get_children( );
                    bool partial_res = true;
                    for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ) && partial_res; ++it )
                    {
                        partial_res = partial_res && sync_in_all_branches( *it, original );
                    }
                    res = partial_res;
                }
            }
        }

        return res;
    }

    bool task_in_loop_is_synchronized_within_loop( Node* task )
    {
        bool res = false;

        Node* task_sync = task->get_children( )[0];
        if( !task_sync->is_omp_virtual_tasksync( ) )
        {
            Node* task_outer = task->get_outer_node( );

            while( ( task_outer != NULL ) && !res )
            {
                // Get the next loop were the task is nested
                while( !task_outer->is_loop_node( ) && ( task_outer != NULL ) )
                    task_outer = task_outer->get_outer_node( );

                if( ( task_outer != NULL ) && ExtensibleGraph::node_contains_node( task_outer, task_sync ) )
                    res = true;
            }
        }

        return res;
    }

    void collect_previous_tasks_synchronized_after_scheduling_point( Node* task, ObjectList<Node*> currents, ObjectList<Node*>& result )
    {
        for( ObjectList<Node*>::iterator it = currents.begin( ); it != currents.end( ); ++it )
        {
            if( !( *it )->is_visited_aux( ) )
            {
                ( *it )->set_visited_aux( true );

                if( ( *it )->is_omp_task_node( ) )
                {
                    Node* it_sync = ( *it )->get_children( )[0];
                    if( ExtensibleGraph::node_is_ancestor_of_node( task, it_sync ) )
                    {
                        result.insert( *it );
                    }
                }
            }
        }
    }
}

    TaskConcurrency::TaskConcurrency( ExtensibleGraph* graph )
        : _graph( graph ), _last_sync( ), _next_sync( )
    {}

    void TaskConcurrency::compute_tasks_concurrency( )
    {
        ObjectList<Node*> tasks = _graph->get_tasks_list( );
        for( ObjectList<Node*>::iterator it = tasks.begin( ); it != tasks.end( ); ++it )
            compute_task_concurrency( *it );
    }

    void TaskConcurrency::compute_task_concurrency( Node* task )
    {
        // Define the immediately previous and next synchronization points
        define_concurrent_regions_limits( task );

        // Compute the regions of code that can be simultaneous with the current tasks
        compute_concurrent_tasks( task );

        // Clean up temporary values for future calls to this method
        _last_sync.clear( );
        _next_sync.clear( );
    }

    void TaskConcurrency::define_concurrent_regions_limits( Node* task )
    {
        // Compute _next_sync
        // When some synchronization point is virtual, then we keep as next_sync the exit node of the graph
        // because this is the most conservative we can do
        // Otherwise, we keep them all
        // -----------------------------------
        ObjectList<Node*> next_syncs_list = task->get_children( );
        ERROR_CONDITION( next_syncs_list.empty( ),
                         "%s: All tasks must have at least one synchronization point but task %d does not have any.",
                         task->get_graph_related_ast( ).get_locus_str( ).c_str( ), task->get_id( ) );

        bool has_post_sync = false;
        for( ObjectList<Node*>::iterator it = next_syncs_list.begin( ); it != next_syncs_list.end( ); ++it )
        {
            if( ( *it )->is_omp_virtual_tasksync( ) )
            {
                has_post_sync = true;
                break;
            }
        }

        if( has_post_sync )
            _next_sync.insert( ExtensibleGraph::get_extensible_graph_from_node( task )->get_graph_exit_node( ) );
        else
            _next_sync = next_syncs_list;

        // Compute _last_sync
        // Common _last_sync will be in the task parents, but when the task is within a loop, then also
        // the children may contain _last_sync due to the iterations
        // The order of the search is important: we first look in the parents and then in the children
        // That is because children may have back edges and we do not want to traverse them twice
        // (from the children and from the parents)
        // -----------------------------------
        find_last_synchronization_point_in_parents( task );
        // FIXME I have to review this code with an example as follows
        //     ...
        //     #pragma omp taskwait
        //     ...
        //     for( i=0; i<N; i++ ) {
        //         #pragma omp task
        //         ...
        //         #pragma omp taskwait
        //     }
        //         if( ExtensibleGraph::node_is_in_loop( task ) )
        //         {
        //             Node* loop = NULL;
        //             Node* task_outer = task->get_outer_node( );
        //             while( task_outer != NULL )
        //             {
        //                 if( task_outer->is_loop_node( ) )
        //                     loop = task_outer;
        //                 task_outer = task_outer->get_outer_node( );
        //             }
        //             ERROR_CONDITION( loop == NULL, "We can't find the loop where the task '%d' is supposed to be embedded\n", task->get_id( ) );
        //
        //             find_last_synchronization_point_in_children( task, task_outer );
        //         }
        ExtensibleGraph::clear_visits_backwards( task );
    }

    void TaskConcurrency::find_last_synchronization_point_in_parents( Node* current )
    {
        if( !current->is_visited( ) )
        {
            current->set_visited( true );

            ObjectList<Node*> parents = current->get_parents( );
            bool keep_looking_for_syncs;
            for( ObjectList<Node*>::iterator it = parents.begin( ); it != parents.end( ); ++it )
            {
                Node* parent = *it;
                keep_looking_for_syncs = true;

                // Check for synchronization in current parent
                if( parent->is_omp_barrier_graph_node( ) || parent->is_omp_taskwait_node( ) )
                {
                    _last_sync.insert( parent );
                    if( ExtensibleGraph::node_is_in_conditional_branch( parent ) )
                    {   // The node is inside a loop|ifelse|switch
                        Node* conditional = NULL;
                        Node* outer = parent->get_outer_node( );
                        while( outer != NULL && conditional == NULL )
                        {
                            if( outer->is_ifelse_statement( ) || outer->is_switch_statement( ) )
                                conditional = outer;
                            outer = outer->get_outer_node( );
                        }
                        if( conditional != NULL )
                        {   // Node is inside ifelse|switch
                            if( !sync_in_all_branches( conditional, conditional ) )
                                keep_looking_for_syncs = false;
                            ExtensibleGraph::clear_visits_aux( parent );
                        }
                    }
                    else
                        keep_looking_for_syncs = false;
                }
                else if( parent->is_graph_node( ) )
                {
                    find_last_synchronization_point_in_parents( parent->get_graph_exit_node( ) );
                }

                // Keep iterating, if necessary
                if( keep_looking_for_syncs )
                {
                    if( parent->is_entry_node( ) )
                    {
                        Node* parent_outer = parent->get_outer_node( );
                        if( parent_outer != NULL )
                        {
                            ObjectList<Node*> outer_parents = parent_outer->get_parents( );
                            for( ObjectList<Node*>::iterator itp = outer_parents.begin( );
                                 itp != outer_parents.end( ) && _last_sync.empty( ); ++itp )
                            {
                                find_last_synchronization_point_in_parents( *itp );
                            }
                        }
                    }
                    else
                    {
                        find_last_synchronization_point_in_parents( parent );
                    }
                }
            }
        }
    }

    // This method is only called when a task (the first recursion parameter) is inside a loop
    // We will stop when we find:
    //    - the loop increment, if exists
    //    - the loop conditional (its False child is the loop node exit)
    void TaskConcurrency::find_last_synchronization_point_in_children( Node* current, Node* loop )
    {
        if( !current->is_visited_aux( ) )
        {
            current->set_visited_aux( true );

            ObjectList<Node*> children = current->get_children( );
            // Since we do not traverse IfElse and Switch statements. the number of children must be <=1
            ERROR_CONDITION( children.size( ) == 1, "Number of children must be 1", 0 );

            Node* child = children[0];

            if( !current->is_exit_node( ) )
            {
                // Check for synchronization in current child
                if( child->is_omp_barrier_graph_node( ) || child->is_omp_taskwait_node( ) )
                {
                    _last_sync.insert( child );
                }
                else
                {
                    if ( child->is_ifelse_statement( ) || child->is_switch_statement( ) )
                    {   // TODO Look for synchronizations in the branches
                        WARNING_MESSAGE( "Found a conditional branch while looking for the Last Synchronization Point "\
                                         "in AutoScoping Phase. We skip any synchronization found here,"\
                                         "but that might be incorrect.\nThis part is under development.", 0 );
                    }
                    else if( child->is_graph_node( ) )
                    {
                        find_last_synchronization_point_in_children( child->get_graph_entry_node( ), loop );
                    }
                }
            }

            // Keep iterating, if necessary
            if( _last_sync.empty( ) )
            {
                if( child->is_exit_node( ) )
                {
                    // Si sortim de la serie de loops on la tasca pot estar nested, ja no hem de buscar més!!
                    // Per això necessitem passar la tasca com a paràmetre d'aquesta funció
                    Node* child_outer = child->get_outer_node( );
                    if( ExtensibleGraph::node_contains_node( loop, child_outer ) )
                        find_last_synchronization_point_in_children( child_outer, loop );
                }
                else
                {
                    find_last_synchronization_point_in_children( child, loop );
                }
            }
        }
    }

    static Node* get_most_outer_parallel(Node* task)
    {
        // Look for the parallel with no immediate inner single
        Node* parallel = NULL;
        std::queue<Node*> queue; queue.push(task);
        while(!queue.empty())
        {
            // Get the first element in the queue
            Node* current = queue.front();
            queue.pop();
            current->set_visited_aux(true);
            
            // Check for the structure we look for
            if(current->is_omp_parallel_node())
            {
                Node* flush = current->get_graph_entry_node()->get_children()[0];
                Node* child = flush->get_children()[0];
                if(!child->is_omp_single_node())
                {   // We found the first parallel with no single
                    parallel = current;
                }
            }
            
            // Enqueue the children of the node we just treated
            ObjectList<Node*> parents;
            if(current->is_graph_node() && !ExtensibleGraph::node_contains_node(current, task))
                parents.insert(current->get_graph_exit_node());
            else if(current->is_entry_node())
            {
                Node* outer = current->get_outer_node();
                if(outer->is_visited_aux())
                    parents = outer->get_parents();
                else
                    parents.insert(outer);
            }
            else
                parents = current->get_parents();
            for(ObjectList<Node*>::iterator it = parents.begin(); it != parents.end(); ++it)
            {
                if(!(*it)->is_visited_aux())
                    queue.push(*it);
            }
        }
        
        // Clear the visits
        ExtensibleGraph::clear_visits_aux_backwards(task);
        
        return parallel;
    }
    
    void TaskConcurrency::compute_concurrent_tasks( Node* task )
    {
        // When the task is in a loop and it is not synchronized inside the loop and
        // it is not synchronized between iterations, it is be concurrent with itself
        Node* skip_task = task;
        if( ExtensibleGraph::node_is_in_loop( task ) &&        // Task is created within a loop
            task_in_loop_is_synchronized_within_loop( task ) )
            skip_task = task;

        // Compute the more accurate last synchronization point if we have not found one
        ObjectList<Node*> last_sync;
        if( !_last_sync.empty( ) )
            last_sync = _last_sync;
        else
        {
            Node* pcfg = ExtensibleGraph::get_extensible_graph_from_node(task);
            Nodecl::NodeclBase related_ast = pcfg->get_graph_related_ast();
            if (related_ast.is<Nodecl::FunctionCode>() && 
                related_ast.get_symbol().get_name() == "main")
            {
                if(IsOmpssEnabled)
                {   // From the beginning, but only tasks created there can be concurrent
                    last_sync.insert(pcfg->get_graph_entry_node());
                }
                else
                {   
                    Node* task_creation = NULL;
                    ObjectList<Node*> task_parents = task->get_parents();
                    for(ObjectList<Node*>::iterator it = task_parents.begin(); it != task_parents.end(); ++it)
                    {
                        if((*it)->is_omp_task_creation_node())
                            task_creation = *it;
                    }
                    ERROR_CONDITION(task_creation==NULL, "No task creation node found for task %s (node %d).\n", 
                                    task->get_graph_related_ast().get_locus_str().c_str(), task->get_id());
                    Node* parallel = get_most_outer_parallel(task_creation);
                    if(parallel != NULL)
                    {   // The most outer parallel without a single: we take the Flush node
                        last_sync.insert(parallel->get_graph_entry_node()->get_children()[0]);
                    }
                    else
                    {   // Cannot assume anything: the last synchronization point is the creation of the task
                        Node* current_outer = task_creation->get_outer_node();
                        Node* most_outer_loop = NULL;
                        while(current_outer != NULL)
                        {
                            if(current_outer->is_loop_node())
                                most_outer_loop = current_outer;
                            current_outer = current_outer->get_outer_node();
                        }
                        if(most_outer_loop != NULL)
                            last_sync.insert(most_outer_loop->get_graph_entry_node());
                        else
                            last_sync.insert(task_creation);
                    }
                }
            }
            else
            {
                last_sync.insert( pcfg->get_graph_entry_node( ) );
            }
        }

        ObjectList<Node*> concurrent_tasks;
        for( ObjectList<Node*>::iterator itl = last_sync.begin( ); itl != last_sync.end( ); ++itl )
        {
            for( ObjectList<Node*>::iterator itn = _next_sync.begin( ); itn != _next_sync.end( ); ++itn )
            {
                collect_tasks_between_nodes( *itl, *itn, skip_task, concurrent_tasks );
                if( ( *itl )->is_omp_taskwait_node( ) )
                {   // Only previous tasks in the same nesting level are synchronized.
                    // We have to add here previous nested tasks
                    ObjectList<Node*> it_parents = ( *itl )->get_parents( );
                    collect_previous_tasks_synchronized_after_scheduling_point( task, it_parents, concurrent_tasks );
                    // We don want to clear the visit in *itl, but from its parents
                    for( ObjectList<Node*>::iterator itp = it_parents.begin( ); itp != it_parents.end( ); ++itp )
                        ExtensibleGraph::clear_visits_backwards( *itp );
                }
            }
        }
        for( ObjectList<Node*>::iterator it = last_sync.begin( ); it != last_sync.end( ); ++it )
            ExtensibleGraph::clear_visits( *it );

        // Set the information computed to the graph
        _graph->add_concurrent_task_group( task, concurrent_tasks );
        _graph->add_last_synchronization( task, last_sync );
        _graph->add_next_synchronization( task, _next_sync );
    }

    // ************************* END class implementing task concurrency analysis ************************* //
    // **************************************************************************************************** //
}
}
}
