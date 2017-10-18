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

#include "cxx-cexpr.h"
#include "tl-datareference.hpp"
#include "tl-pcfg-utils.hpp"
#include "tl-alias-analysis.hpp"
#include "tl-task-sync.hpp"
#include "tl-tribool.hpp"

#include <queue>

namespace TL {
namespace Analysis {
namespace TaskAnalysis{

namespace {
    // #define TASK_SYNC_DEBUG

    std::map<Node*, ObjectList<Nodecl::NodeclBase> > task_matched_src_deps;
    std::set<Node*> dead_tasks_before_sync;

    bool function_waits_tasks(TL::Symbol sym)
    {
        scope_entry_t* entry = sym.get_internal_symbol();

        int i;
        for (i = 0; i < symbol_entity_specs_get_num_gcc_attributes(entry); i++)
        {
            const char* attr_name = symbol_entity_specs_get_gcc_attributes_num(entry, i).attribute_name;
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
            bool keep_alive,
            Node* current,
            Node* current_sync_point)
    {
        switch (task_sync_rel.value())
        {
            case tribool::unknown :
            case tribool::yes :
                {
                    SyncKind sync_kind;
                    if (task_sync_rel == tribool::yes)
                        sync_kind = __Static;
                    else if (task_sync_rel == tribool::unknown)
                        sync_kind = __Maybe;
                    else
                        internal_error("Code unreachable", 0);

#ifdef TASK_SYNC_DEBUG
                    if (points_of_sync.find(alive_tasks_it->node) != points_of_sync.end())
                    {
                        std::cerr << __FILE__ << ":" << __LINE__
                            << " task (among others) maybe synchronizes in this task execution" << std::endl;
                    }
                    else
                    {
                        std::cerr << __FILE__ << ":" << __LINE__
                            << " task maybe synchronizes in this task execution" << std::endl;
                    }
#endif
                    // Since the algorithm iterates until no changes,
                    // it may happen that the previous iteration has decided
                    // this synchronization is of a different kind
                    // In that case, we do not have to add it again
                    bool already_a_point_of_sync = false;
                    PointOfSyncList points_of_sync_list = points_of_sync[alive_tasks_it->node];
                    for (PointOfSyncList::iterator it = points_of_sync_list.begin();
                         it != points_of_sync_list.end(); ++it)
                    {
                        if (it->first == current_sync_point) {
                            already_a_point_of_sync = true;
                            break;
                        }
                    }
                    if (!already_a_point_of_sync)
                        points_of_sync[alive_tasks_it->node].insert(std::make_pair(current_sync_point, sync_kind));

                    if (task_sync_rel == tribool::yes && !keep_alive)
                    {
#ifdef TASK_SYNC_DEBUG
                        std::cerr << __FILE__ << ":" << __LINE__ << " but we know it statically synchronizes" << std::endl;
#endif
                    }
                    else
                    {
#ifdef TASK_SYNC_DEBUG
                        std::pair<AliveTaskSet::iterator, bool> res =
#endif
                        // Note: this statement was previously done for both tribool::yes and tribool::unknown
                        //       but we do not see reason for doing that anymore
                        //       Otherwise, we may have 'false' synchronizations between tasks
                        current->get_live_out_tasks().insert(*alive_tasks_it);
#ifdef TASK_SYNC_DEBUG
                        if (res.second)
                        {
                                std::cerr << __FILE__ << ":" << __LINE__
                                << " but it is (potentially) still alive after execution" << std::endl;
                        }
#endif
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

#ifdef TASK_SYNC_DEBUG
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
#endif

    // NOTE: we check only for array/class to disregard pointers _and_ references
    bool is_strict_subobject_access(NBase& data_ref)
    {
        if (data_ref.is<Nodecl::Symbol>())
            return false;

        NBase current = data_ref.no_conv();
        while (!current.is<Nodecl::Symbol>())
        {
            if (current.is<Nodecl::ArraySubscript>())
            {
                NBase subscripted = current.as<Nodecl::ArraySubscript>().get_subscripted().no_conv();
                if (subscripted.is<Nodecl::Symbol>())
                {   // a[x] where 'a' should be an array
                    if (!subscripted.get_symbol().get_type().is_array())
                        return false;
                }
                else if (subscripted.is<Nodecl::ClassMemberAccess>())
                {   // (b.a)[x]
                    NBase member = subscripted.as<Nodecl::ClassMemberAccess>().get_member();
                    ERROR_CONDITION(!member.is<Nodecl::Symbol>(), "This is not a valid ClassMemberAccess", 0);

                    if (!member.get_symbol().get_type().is_array())
                        return false;
                }

                current = subscripted;
            }
            else if (current.is<Nodecl::ClassMemberAccess>())
            {
                NBase lhs = current.as<Nodecl::ClassMemberAccess>().get_lhs().no_conv();
                if (lhs.is<Nodecl::Symbol>())
                {   // a.b
                    if (!lhs.get_symbol().get_type().is_class())
                        return false;
                }
                else if (lhs.is<Nodecl::ClassMemberAccess>())
                {   // a.b.c
                    NBase member = lhs.as<Nodecl::ClassMemberAccess>().get_member();
                    ERROR_CONDITION(!member.is<Nodecl::Symbol>(), "This is not a valid ClassMemberAccess", 0);

                    if (!member.get_symbol().get_type().is_class())
                        return false;
                }

                current = lhs;
            }
            else if (current.is<Nodecl::Conversion>())
            {
                internal_error("Unexpected node", 0);
            }
            else
            {
                return false;
            }
        }

        return true;
    }

    tribool may_have_dependence(const NBase& source, const NBase& target)
    {
        TL::DataReference source_data_ref(source);
        TL::DataReference target_data_ref(target);
        
        // 1.- Some data reference is not valid => return unknown
        if (!source_data_ref.is_valid()
                || !target_data_ref.is_valid())
            return tribool();

        // 2.- The two data references are simple objects => 
        //     - return true when the two symbols are the same
        //     - return false otherwise
        TL::Symbol source_sym = source_data_ref.get_base_symbol();
        TL::Symbol target_sym = target_data_ref.get_base_symbol();

        bool source_is_symbol = source.is<Nodecl::Symbol>()
            && !source.get_symbol().get_type().is_any_reference();
        bool target_is_symbol = target.is<Nodecl::Symbol>()
            && !target.get_symbol().get_type().is_any_reference();

        if (source_is_symbol && target_is_symbol)
        {
            // If both data references are simple objects, different names means different symbols
            return (source_sym == target_sym) ? tribool::yes : tribool::no;
        }

        // 3.- The two access are either sub-objects or shaping expressions (specifying the whole object or a sub-part of it)
        //     - return true when the base symbol is the same in both accesses
        //     - return unknown when the base symbol may have aliasing
        //     - return false otherwise
        bool source_is_object = source.is<Nodecl::Shaping>();
        bool target_is_object = target.is<Nodecl::Shaping>();
        bool source_is_subobject = is_strict_subobject_access(source_data_ref);
        bool target_is_subobject = is_strict_subobject_access(target_data_ref);

        if (source_is_object && target_is_object)
        {   // Check the symbols (which shall be pointers, and check whether this pointers are restrict)
            Type source_type = source_sym.get_type();
            Type target_type = target_sym.get_type();

            if (source_sym == target_sym)
            {
                if (source_type.is_restrict())
                {
                    return tribool::yes;
                }
                else
                {
                    return tribool::unknown;
                }
            }
            else
            {
                if (source_type.is_restrict())
                {
                    return tribool::no;
                }
                else
                {
                    return tribool::unknown;
                }
            }
        }
        if (source_is_subobject && target_is_subobject)
        {
            // If one is object and the other sub-object (or both sub-objects),
            // if the base symbol is different, they they cannot be the same dependence
            if (source_sym != target_sym)
            {
                return tribool::no;
            }
            else
            {
                if (!source_is_subobject || !target_is_subobject)
                    return tribool::yes;
            }
        }

        // 4.- In all other cases we take a conservative stance
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

    bool is_only_input_dependence(NBase n)
    {
        return n.is<Nodecl::OpenMP::DepIn>();
    }

    tribool compute_taskwait_sync_relationship(Node* source, Node* target)
    {
        // Source (task)
        NBase task_node_source = source->get_graph_related_ast();
        ERROR_CONDITION(task_node_source.is_null(), "Invalid source task tree", 0);
        ERROR_CONDITION(!task_node_source.is<Nodecl::OpenMP::Task>()
                && !task_node_source.is<Nodecl::OmpSs::TaskExpression>()
                && !task_node_source.is<Nodecl::OmpSs::TaskCall>(),
                "Expecting an OpenMP::Task, OmpSs::TaskExpression or OmpSs::TaskCall source node here got a %s",
                ast_print_node_type(task_node_source.get_kind()));
        if (task_node_source.is<Nodecl::OmpSs::TaskExpression>())
        {
            // Return unknown
            return tribool();
        }

        // The task has already been synchronized with other tasks
        // where the target task has an input dependence with some out of the source
        if (dead_tasks_before_sync.find(source) != dead_tasks_before_sync.end())
        {
            return tribool::no;
        }

        Nodecl::List task_source_env;
        if (task_node_source.is<Nodecl::OpenMP::Task>())
        {
            Nodecl::OpenMP::Task task_source(task_node_source.as<Nodecl::OpenMP::Task>());
            task_source_env = task_source.get_environment().as<Nodecl::List>();
        }
        else if (task_node_source.is<Nodecl::OmpSs::TaskCall>())
        {
            Nodecl::OmpSs::TaskCall task_source(task_node_source.as<Nodecl::OmpSs::TaskCall>());
            task_source_env = task_source.get_environment().as<Nodecl::List>();
        }
        else
        {
            internal_error("Code unreachable", 0);
        }

        // TODO - Concurrent is not handled yet
        // TODO - OpenMP::Base may create more than ONE dep_xxx tree (here we would overwrite them)
        Nodecl::NodeclBase source_dep_in;
        Nodecl::NodeclBase source_dep_out;
        Nodecl::NodeclBase source_dep_inout;
        Nodecl::NodeclBase source_dep_commutative;
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
            else if (it->is<Nodecl::OmpSs::DepCommutative>())
                source_dep_commutative = *it;
        }

        // Target (taskwait)
        ERROR_CONDITION(target->get_type() != __OmpWaitonDeps, "Invalid node", 0);
        NodeclList task_node_target_stmts = target->get_statements();
        ERROR_CONDITION(task_node_target_stmts.size() != 1, "Invalid list of statements", 0);
        NBase taskwait_node_target = task_node_target_stmts[0];
        ERROR_CONDITION(taskwait_node_target.is_null(), "Invalid target task tree", 0);
        ERROR_CONDITION(!taskwait_node_target.is<Nodecl::OpenMP::Taskwait>(),
                "Expecting an OpenMP::Taskwait target node here got a %s",
                ast_print_node_type(taskwait_node_target.get_kind()));

        Nodecl::List task_target_env = taskwait_node_target
            .as<Nodecl::OpenMP::Taskwait>()
            .get_environment()
            .as<Nodecl::List>();

        NBase target_dep_in;
        NBase target_dep_out;
        NBase target_dep_inout;
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
        Nodecl::NodeclBase sources[] = { source_dep_in, source_dep_inout, source_dep_out };
        int num_sources = sizeof(sources)/sizeof(sources[0]);
        Nodecl::NodeclBase targets[] = { target_dep_in, target_dep_inout, target_dep_out };
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
                     // Note we (ab)use the fact that DepIn/DepOut/DepInOut all have the
                     // same physical layout
                     && may_have_dependence_list(
                         sources[n_source].as<Nodecl::OpenMP::DepOut>().get_exprs().as<Nodecl::List>(),
                         targets[n_target].as<Nodecl::OpenMP::DepIn>().get_exprs().as<Nodecl::List>()));
            }
        }

        return may_have_dep;
    }

    // Computes if task source will synchronize with the creation of the task target
    tribool compute_task_sync_relationship(Node* source, Node* target, bool keep_alive)
    {
#ifdef TASK_SYNC_DEBUG
        std::cerr << "CHECKING DEPENDENCES STATICALLY " << source->get_id() << " -> " << target->get_id() << std::endl;
#endif

        // TL::NodeclList source_statements = source->get_statements();
        // ERROR_CONDITION(source_statements.empty(), "Invalid source statement set", 0);
        NBase task_node_source = source->get_graph_related_ast();
        ERROR_CONDITION(task_node_source.is_null(), "Invalid source task tree", 0);
        ERROR_CONDITION(!task_node_source.is<Nodecl::OpenMP::Task>()
                && !task_node_source.is<Nodecl::OpenMP::Target>()
                && !task_node_source.is<Nodecl::OmpSs::TaskExpression>()
                && !task_node_source.is<Nodecl::OmpSs::TaskCall>(),
                "Expecting an OpenMP::Task, OpenMP::Target, "
                "OmpSs::TaskExpression or OmpSs::TaskCall source node here got a %s",
                ast_print_node_type(task_node_source.get_kind()));
        Nodecl::List task_source_env;
        if (task_node_source.is<Nodecl::OpenMP::Task>()
            || task_node_source.is<Nodecl::OpenMP::Target>())
        {   // The structure of the Task and Target nodes is the same
            Nodecl::OpenMP::Task task_source(task_node_source.as<Nodecl::OpenMP::Task>());
            task_source_env = task_source.get_environment().as<Nodecl::List>();
        }
        else if (task_node_source.is<Nodecl::OmpSs::TaskCall>())
        {
            Nodecl::OmpSs::TaskCall task_source(task_node_source.as<Nodecl::OmpSs::TaskCall>());
            task_source_env = task_source.get_site_environment().as<Nodecl::List>();
        }
        else if (task_node_source.is<Nodecl::OmpSs::TaskExpression>())
        {
            // Return unknown
            return tribool();
        }
        else
        {
            internal_error("Code unreachable", 0);
        }

        // FIXME: Extend support for concurrent dependencies
        Nodecl::List in_sources, out_sources;
        for (Nodecl::List::iterator it = task_source_env.begin();
                it != task_source_env.end();
                it++)
        {
            if (it->is<Nodecl::OpenMP::DepIn>())
                in_sources.append(it->as<Nodecl::OpenMP::DepIn>().get_exprs().shallow_copy());
            else if (it->is<Nodecl::OpenMP::DepOut>())
                out_sources.append(it->as<Nodecl::OpenMP::DepOut>().get_exprs().shallow_copy());
            else if (it->is<Nodecl::OpenMP::DepInout>() || it->is<Nodecl::OmpSs::DepCommutative>()) {
                in_sources.append(it->as<Nodecl::OpenMP::DepInout>().get_exprs().shallow_copy());
                out_sources.append(it->as<Nodecl::OpenMP::DepInout>().get_exprs().shallow_copy());
            }
        }

        // TL::NodeclList target_statements = target->get_statements();
        // ERROR_CONDITION(target_statements.empty(), "Invalid target statement set", 0);
        NBase task_node_target = target->get_graph_related_ast();
        ERROR_CONDITION(task_node_source.is_null(), "Invalid target task tree", 0);
        ERROR_CONDITION(!task_node_target.is<Nodecl::OpenMP::Task>()
                && !task_node_target.is<Nodecl::OpenMP::Target>()
                && !task_node_target.is<Nodecl::OmpSs::TaskExpression>()
                && !task_node_target.is<Nodecl::OmpSs::TaskCall>(),
                        "Expecting an OpenMP::Task, OpenMP::Target, "
                        "OmpSs::TaskExpression or OmpSs::TaskCall target node here got a %s",
                ast_print_node_type(task_node_target.get_kind()));
        Nodecl::List task_target_env;
        if (task_node_target.is<Nodecl::OpenMP::Task>()
            || task_node_target.is<Nodecl::OpenMP::Target>())
        {   // The structure of the Task and Target nodes is the same
            Nodecl::OpenMP::Task task_target(task_node_target.as<Nodecl::OpenMP::Task>());
            task_target_env = task_target.get_environment().as<Nodecl::List>();
        }
        else if (task_node_target.is<Nodecl::OmpSs::TaskCall>())
        {
            Nodecl::OmpSs::TaskCall task_target(task_node_target.as<Nodecl::OmpSs::TaskCall>());
            task_target_env = task_target.get_site_environment().as<Nodecl::List>();
        }
        else if (task_node_target.is<Nodecl::OmpSs::TaskExpression>())
        {
            // Return unknown
            return tribool();
        }
        else
        {
            internal_error("Code unreachable", 0);
        }

        Nodecl::List all_targets, out_targets;
        for (Nodecl::List::iterator it = task_target_env.begin();
                it != task_target_env.end();
                it++)
        {
            if (it->is<Nodecl::OpenMP::DepIn>())
                all_targets.append(it->as<Nodecl::OpenMP::DepIn>().get_exprs().shallow_copy());
            else if (it->is<Nodecl::OpenMP::DepOut>()
                    || it->is<Nodecl::OpenMP::DepInout>()
                    || it->is<Nodecl::OmpSs::DepCommutative>()) {
                all_targets.append(it->as<Nodecl::OpenMP::DepOut>().get_exprs().shallow_copy());
                out_targets.append(it->as<Nodecl::OpenMP::DepOut>().get_exprs().shallow_copy());
            }
        }

        tribool may_have_dep = tribool::no;

        // DRY
        // Match source inputs with target outputs
        for (Nodecl::List::iterator its = in_sources.begin(); its != in_sources.end(); ++its)
        {
            for (Nodecl::List::iterator itt = out_targets.begin(); itt != out_targets.end(); ++itt)
            {
                may_have_dep = may_have_dep || may_have_dependence(*its, *itt);
            }
        }

        // Match source outputs with target inputs and outputs
        task_matched_src_deps.insert(std::pair<Node*, ObjectList<Nodecl::NodeclBase> >(source, in_sources.to_object_list()));
        bool all_sources_are_inputs = false, all_targets_are_inputs = true;
        for (Nodecl::List::iterator its = out_sources.begin(); its != out_sources.end(); ++its)
        {
            tribool pair_may_have_dep = tribool::no;
            for (Nodecl::List::iterator itt = all_targets.begin(); itt != all_targets.end(); ++itt)
            {
                pair_may_have_dep = pair_may_have_dep || may_have_dependence(*its, *itt);
                if (pair_may_have_dep == tribool::yes || pair_may_have_dep == tribool::unknown)
                {
                    if (Nodecl::Utils::list_contains_nodecl_by_structure(out_targets.to_object_list(), *itt))
                        all_targets_are_inputs = false;
                }
            }

            if (pair_may_have_dep == tribool::yes)
            {
                task_matched_src_deps[source].erase(
                        std::remove(task_matched_src_deps[source].begin(),
                                    task_matched_src_deps[source].end(),
                                    *its),
                        task_matched_src_deps[source].end());
            }
            may_have_dep = may_have_dep || pair_may_have_dep;
        }

        if (may_have_dep == tribool::no)
            return may_have_dep;

        // This tasks will be dead when the next taskwait/barrier is encountered
        // but we have to keep them alive since they may sincronize in other task
        if (all_targets_are_inputs && may_have_dep == tribool::yes)
        {
            dead_tasks_before_sync.insert(source);
        }

        // It may happen that, even though a task is synchronized for sure in a nother task,
        // we may want to keep it alive. This happens when:
        // * An output dependence has been synchronized with an input dependence
        //        => other posterior inputs/outputs may need to synchronize with the same source
        // * There are still dependences in the source that have not match
        //        => other posterior dependences may match with those previously unmatched
        if ((!all_sources_are_inputs && all_targets_are_inputs)
            || !task_matched_src_deps[source].empty())
        {
            keep_alive = true;
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

            // If the task has already been synchronized with other tasks
            // where the target task has an input dependence with some out of the source
            // then nothing to do here
            if (dead_tasks_before_sync.find(alive_tasks_it->node) != dead_tasks_before_sync.end())
            {
                // If there is only one child,
                // change the synchronization type from maybe to yes
                PointOfSyncList alive_tasks_it_syncs = points_of_sync[alive_tasks_it->node];
                if (alive_tasks_it_syncs.size() == 1)
                {
                    Node* n = alive_tasks_it_syncs[0].first;
                    PointOfSyncList new_list(1, std::pair<Node*, SyncKind>(n, __Static));
                    points_of_sync[alive_tasks_it->node] = new_list;
                }
                continue;
            }

            points_of_sync[alive_tasks_it->node].insert(std::make_pair(current, __Static));
        }

        // Remove all the tasks that do sync here
        AliveTaskSet still_alive;
        for (AliveTaskSet::iterator alive_tasks_it = current->get_live_in_tasks().begin();
                alive_tasks_it != current->get_live_in_tasks().end();
                alive_tasks_it++)
        {
            if (alive_tasks_it->domain == current_domain_id)
                continue;

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
         }

        AliveTaskSet initial_alive_out = current->get_live_out_tasks();

#ifdef TASK_SYNC_DEBUG
        std::cerr << "["
                    << current->get_id()
                    << ":" << current->get_type_as_string()
                    << ":" << (current->is_graph_node() ? current->get_graph_type_as_string() : "")
                    << "]"
                    << "Before" << std::endl
                    << "  IN[alive] = " << print_set(current->get_live_in_tasks()) << std::endl
                    << "  OUT[alive] = " << print_set(current->get_live_out_tasks()) << std::endl;
#endif

        if (current->is_omp_task_node()
            || current->is_omp_async_target_node())
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
                // If the task has already been synchronized with other tasks
                // where the target task has an input dependence with some out of the source
                // then nothing to do here
                if (dead_tasks_before_sync.find(alive_tasks_it->node) != dead_tasks_before_sync.end())
                {
                    // If there is only one child,
                    // change the synchronization type from maybe to yes
                    PointOfSyncList alive_tasks_it_syncs = points_of_sync[alive_tasks_it->node];
                    if (alive_tasks_it_syncs.size() == 1)
                    {
                        Node* n = alive_tasks_it_syncs[0].first;
                        PointOfSyncList new_alive_tasks_it_syncs(1, std::pair<Node*, SyncKind>(n, __Static));
                        points_of_sync[alive_tasks_it->node] = new_alive_tasks_it_syncs;
                    }
                    continue;
                }

                points_of_sync[alive_tasks_it->node].insert(std::make_pair(current, __Static));
            }
            current->get_live_out_tasks().clear();
        }
        else if (current->is_graph_node())
        {
            Node* graph_entry = current->get_graph_entry_node();
            graph_entry->get_live_in_tasks() = current->get_live_in_tasks();
            // graph_entry->get_static_sync_in_tasks() = current->get_static_sync_in_tasks();

            compute_task_synchronizations_rec(graph_entry, changed, points_of_sync, current_domain_id, next_domain_id);

            Node* graph_exit = current->get_graph_exit_node();
            current->get_live_out_tasks() = graph_exit->get_live_out_tasks();
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
                tribool task_sync_rel;
                if (alive_tasks_it->domain == current_domain_id)
                {
                    task_sync_rel = compute_taskwait_sync_relationship(alive_tasks_it->node, current);
                }
                else
                {
                    task_sync_rel = tribool::no;
                }
                set_sync_relationship(task_sync_rel, alive_tasks_it, points_of_sync, /*keep_alive*/false, current, current);
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
                tribool task_sync_rel;
                bool keep_alive = false;
                if (alive_tasks_it->domain == current_domain_id)
                {
                    task_sync_rel = compute_task_sync_relationship(alive_tasks_it->node, task, keep_alive);
                }
                else
                {
                    task_sync_rel = tribool::no;
                }
                set_sync_relationship(task_sync_rel, alive_tasks_it, points_of_sync, keep_alive, current, task);
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
        else if (current->is_function_call_node()
                && current->get_function_node_symbol().is_valid()
                && function_waits_tasks(current->get_function_node_symbol()))
        {
            shallow_synchronization_point(current, current_domain_id, points_of_sync);
        }
        else
        {
            // All other nodes just propagate OUT(X) = IN(X)
            current->get_live_out_tasks() = current->get_live_in_tasks();
        }

        if (initial_alive_out != current->get_live_out_tasks())
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
            << "  OUT[alive] = " << print_set(current->get_live_out_tasks()) << std::endl;
#endif

        ObjectList<Edge*> exit_edges = current->get_exit_edges();
        for (ObjectList<Edge*>::iterator edge_it = exit_edges.begin();
                edge_it != exit_edges.end();
                edge_it++)
        {
            compute_task_synchronizations_rec((*edge_it)->get_target(), changed, points_of_sync, current_domain_id, next_domain_id);
        }
    }

    void purge_exit_live_in_tasks_rec(
            Node* current,
            AliveTaskSet& exit_live_in_tasks)
    {
        if (current->is_visited())
            return;

        current->set_visited(true);

        // Treat the current node
        if (current->is_graph_node())
        {
            if (current->is_omp_task_node())
            {
                // Look for the task in the set of alive tasks
                // We cannot take advantage of the set structure
                // because the elements are AliveTaskItem
                for (AliveTaskSet::iterator it = exit_live_in_tasks.begin();
                     it != exit_live_in_tasks.end(); )
                {
                    if (it->node == current
                        && dead_tasks_before_sync.find(it->node) != dead_tasks_before_sync.end())
                    {
                        // Remove the task form the set of alive tasks
                        // because it has already been synchronized
                        exit_live_in_tasks.erase(it++);

                        // Additionally, if there is only one child,
                        // change the synchronization type from maybe to yes
                        ObjectList<Edge*> exits = current->get_exit_edges();
                        if (exits.size() == 1)
                        {
                            exits[0]->set_sync_kind(__Static);
                            const char* s = exits[0]->get_sync_kind_as_string();
                            exits[0]->set_label(Nodecl::StringLiteral::make(
                                    Type(get_literal_string_type(strlen(s)+1, get_char_type())),
                                    const_value_make_string(s, strlen(s))));
                        }
                    }
                    else
                    {
                        ++it;
                    }
                }
            }

            purge_exit_live_in_tasks_rec(current->get_graph_entry_node(), exit_live_in_tasks);
        }

        // Keep iterating
        const ObjectList<Node*>& children = current->get_children();
        for (ObjectList<Node*>::const_iterator it = children.begin();
             it != children.end(); ++it)
        {
            purge_exit_live_in_tasks_rec(*it, exit_live_in_tasks);
        }
    }

    void compute_condition_for_unmatched_values(
        Node* n_node, Node* m_node,
        const NBase& n, const NBase& m,
        NBase& condition)
    {
        // Get the condition for the two values
        NBase cond_part;
        Type t = n.get_type();
        if (n.is<Nodecl::Range>())
        {
            if (m.is<Nodecl::Range>())
            {   // n=[lb1, ub1], m=[lb2, ub2]
                Nodecl::Range n_range = n.as<Nodecl::Range>();
                Nodecl::Range m_range = m.as<Nodecl::Range>();
                cond_part = Nodecl::LogicalOr::make(
                    Nodecl::LogicalAnd::make(   // lb2 <= lb1 <= ub2
                        Nodecl::GreaterOrEqualThan::make(
                            n_range.get_lower().shallow_copy(), m_range.get_lower().shallow_copy(), t),
                        Nodecl::LowerOrEqualThan::make(
                            n_range.get_lower().shallow_copy(), m_range.get_upper().shallow_copy(), t),
                        t),
                    Nodecl::LogicalAnd::make(   // lb1 <= lb2 <= ub1
                        Nodecl::LowerOrEqualThan::make(
                            n_range.get_lower().shallow_copy(), m_range.get_lower().shallow_copy(), t),
                        Nodecl::GreaterOrEqualThan::make(
                            n_range.get_upper().shallow_copy(), m_range.get_lower().shallow_copy(), t),
                        t),
                    t
                );
            }
            else
            {   // n=[lb1, ub1], m=[v]
                cond_part = Nodecl::LogicalAnd::make(
                    Nodecl::LowerOrEqualThan::make(n.as<Nodecl::Range>().get_lower().shallow_copy(), m.shallow_copy(), t),
                    Nodecl::LowerOrEqualThan::make(m.shallow_copy(), n.as<Nodecl::Range>().get_lower().shallow_copy(), t),
                    t
                );
            }
        }
        else
        {
            if (m.is<Nodecl::Range>())
            {   // n=[v], m=[lb1, ub1]
                cond_part = Nodecl::LogicalAnd::make(
                    Nodecl::LowerOrEqualThan::make(m.as<Nodecl::Range>().get_lower().shallow_copy(), n.shallow_copy(), t),
                    Nodecl::LowerOrEqualThan::make(n.shallow_copy(), m.as<Nodecl::Range>().get_lower().shallow_copy(), t),
                    t
                );
            }
            else
            {   // n=[v1], m=[v2]
                cond_part = Nodecl::Equal::make(n.shallow_copy(), m.shallow_copy(), n.get_type());
            }
        }

        // Rebuild the condition composing the old condition and the new computed part
        if (condition.is_null())
            condition = cond_part;
        else if (!cond_part.is_null())
            condition = Nodecl::LogicalAnd::make(condition.shallow_copy(), cond_part, condition.get_type());
    }

    void match_array_subscripts(
        Node* n_node, Node* m_node,
        const Nodecl::List& n_subs, const Nodecl::List& m_subs,
        NBase& condition)
    {
        Nodecl::List::iterator itn = n_subs.begin();
        Nodecl::List::iterator itm = m_subs.begin();
        for (; (itn != n_subs.end() && itm != m_subs.end()); ++itn, ++itm)
        {
            compute_condition_for_unmatched_values(n_node, m_node, *itn, *itm, condition);
        }
    }

    void match_dependence(
        Node* n_node, Node* m_node,
        const NBase& n, const NBase& m,
        NBase& condition)
    {
        Nodecl::NodeclBase n_ = n.no_conv();
        Nodecl::NodeclBase m_ = m.no_conv();

        // Skip shaping nodes
        if (n_.is<Nodecl::Shaping>())
            return match_dependence(n_node, m_node, n_.as<Nodecl::Shaping>().get_postfix(), m_, condition);
        if (m_.is<Nodecl::Shaping>())
            return match_dependence(n_node, m_node, n_, m_.as<Nodecl::Shaping>().get_postfix(), condition);

        // Compare the two dependencies
        if (n_.is<Nodecl::ClassMemberAccess>() && m_.is<Nodecl::ClassMemberAccess>())
        {
            Nodecl::ClassMemberAccess src_dep_ = n_.as<Nodecl::ClassMemberAccess>();
            Nodecl::ClassMemberAccess tgt_dep_ = m_.as<Nodecl::ClassMemberAccess>();
            if (Nodecl::Utils::structurally_equal_nodecls(src_dep_.get_lhs(), tgt_dep_.get_lhs()))
            {
                match_dependence(n_node, m_node,
                                 src_dep_.get_member(), src_dep_.get_member(),
                                 condition);
            }
        }
        else if (n_.is<Nodecl::ArraySubscript>() && m_.is<Nodecl::ArraySubscript>())
        {
            const Nodecl::ArraySubscript& m_arr = m_.as<Nodecl::ArraySubscript>();
            const Nodecl::ArraySubscript& n_arr = n_.as<Nodecl::ArraySubscript>();

            // Check the base refers to the same object
            const NBase& m_base = m_arr.get_subscripted();
            const NBase& n_base = n_arr.get_subscripted();
            if (!Nodecl::Utils::structurally_equal_nodecls(m_base, n_base, /*skip_conversions*/ true))
            {
                tribool there_exists_alias = accesses_may_be_alias(n_, m_);
                if (there_exists_alias.is_true())
                {   // The condition depend on the bases to point to the same memory location
                    condition = Nodecl::Equal::make(m_base.shallow_copy(), n_base.shallow_copy(), n.get_type());
                }
            }
            else
            {
                match_array_subscripts(n_node, m_node,
                                       n_arr.get_subscripts().as<Nodecl::List>(),
                                       m_arr.get_subscripts().as<Nodecl::List>(),
                                       condition);
            }
        }
    }
}

    TaskSynchronizations::TaskSynchronizations(ExtensibleGraph* graph, bool is_ompss_enabled)
        : _graph(graph)
    {}

    void TaskSynchronizations::compute_task_synchronization_labels()
    {
        Node *root = _graph->get_graph();

        PointsOfSync points_of_sync;

        bool changes;
        do
        {
            changes = false;
            int next_domain_id = 1;
            compute_task_synchronizations_rec(root, changes, points_of_sync,
                    /* current_domain_id */ 0, next_domain_id);
            ExtensibleGraph::clear_visits(root);
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
            // Note: We need to preserve reverse order for Range Analysis correctness
            for (PointOfSyncList::reverse_iterator jt = it->second.rbegin();
                    jt != it->second.rend();
                    jt++)
            {
#ifdef TASK_SYNC_DEBUG
                std::cerr << "CONNECTING " << it->first->get_id() << " -> " << (*jt).first->get_id() << std::endl;
#endif
                Edge* edge = _graph->connect_nodes(it->first, (*jt).first, __Always,
                                                   NBase::null(), /*is task edge*/ true);
                edge->set_sync_kind((*jt).second);
                const char* s = edge->get_sync_kind_as_string();
                edge->set_label(Nodecl::StringLiteral::make(Type(get_literal_string_type(strlen(s)+1, get_char_type() )),
                                                            const_value_make_string(s, strlen(s))));
            }
        }

        // Eliminate from the alive tasks those tasks that
        // have been synchronized with one or more tasks when the target task has an input dependence.
        // We keep them alive in case there are more tasks with a dependency on the same object
        // but at the end of the graph we know for sure they are synchronized.
        Node* exit = root->get_graph_exit_node();
        AliveTaskSet exit_live_in_tasks = exit->get_live_in_tasks();
        purge_exit_live_in_tasks_rec(root, exit_live_in_tasks);
        ExtensibleGraph::clear_visits(root);

        // If there are tasks alive at the end of the graph,
        // connect them to the virtual synchronization point
        // Otherwise, return
        if (exit_live_in_tasks.empty())
            return;
        Node* post_sync = _graph->create_unconnected_node(__OmpVirtualTaskSync, NBase::null());
        _graph->set_post_sync(post_sync);
        for (AliveTaskSet::iterator it = exit_live_in_tasks.begin();
                it != exit_live_in_tasks.end();
                it++)
        {
#ifdef TASK_SYNC_DEBUG
            std::cerr << "CONNECTING " << it->node->get_id() << " -> " << post_sync->get_id() << std::endl;
#endif
            Edge* edge = _graph->connect_nodes(it->node, post_sync, __Always,
                    NBase::null(), /*is task edge*/ true);

            edge->set_sync_kind(__Post);
            const char* s = edge->get_sync_kind_as_string();
            edge->set_label(Nodecl::StringLiteral::make(Type(get_literal_string_type(strlen(s)+1, get_char_type())),
                                                        const_value_make_string(s, strlen(s))));
        }
    }

    void TaskSynchronizations::compute_task_synchronization_conditions_rec(Node* current)
    {
        if (current->is_visited())
            return;

        current->set_visited(true);

        // Treat the current node
        if (current->is_graph_node())
        {
            // Treat the inner nodes recursively
            compute_task_synchronization_conditions_rec(current->get_graph_entry_node());

            // If the current node is a task or a target,
            // then try to simplify its synchronizations
            if (current->is_omp_task_node()
                || current->is_omp_async_target_node())
            {
                // Tune the synchronizations with its children, if possible
                ObjectList<Edge*> exits = current->get_exit_edges();
                for (ObjectList<Edge*>::iterator it = exits.begin(); it != exits.end(); ++it)
                {
                    if ((*it)->get_sync_kind() == __Maybe)
                    {   // Can we tune this edge to make it static
                        // if so, remove the rest of the edges
                        NBase target_task_environ = (*it)->get_target()->get_graph_related_ast().as<Nodecl::OpenMP::Task>().get_environment();
                        NBase condition = match_dependencies(current, (*it)->get_target());
                        // This information will be used when building the Task Dependency Graph
                        (*it)->set_condition(condition);
                    }
                }
            }
        }

        // Treat the children recursively
        ObjectList<Node*> children = current->get_children();
        for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
        {
            compute_task_synchronization_conditions_rec(*it);
        }
    }

    void TaskSynchronizations::compute_task_synchronization_conditions()
    {
        Node* entry = _graph->get_graph()->get_graph_entry_node();
        compute_task_synchronization_conditions_rec(entry);
        ExtensibleGraph::clear_visits(entry);
    }

    void TaskSynchronizations::compute_task_synchronizations()
    {
        compute_task_synchronization_labels();
        compute_task_synchronization_conditions();
    }

    template <typename DependenceNode>
    static TL::ObjectList<Nodecl::NodeclBase> gather_dependences(
        Nodecl::List list,
        Nodecl::NodeclBase (DependenceNode::*get_dependences)() const)
    {
        ObjectList<NBase> deps = list.find_all<DependenceNode>()
            .template map<NBase>(get_dependences)                            // ObjectList<NBase>
            .template map<Nodecl::List>(&NBase::as<Nodecl::List>)            // ObjectList<Nodecl::List>
            .template map<ObjectList<NBase> >(&Nodecl::List::to_object_list) // ObjectList<ObjectList<NBase> >
            .reduction(append_two_lists<NBase>);                             // ObjectList<NBase>

        return deps;
    }

    /*!This method returns the condition that has to be associated to an edge of type 'maybe' that connects two tasks which:
     * \param source_environ is the environment of the source task
     * \param target_environ is the environment of the target task
     * Two cases may happen:
     * - The edge is determined to be 'static', meaning that the target task depends for sure of the source task.
     *   In this case, the return value is a null NodeclBase
     * - The edge cannot be determined to be 'static'.
     *   In this case, the return value is the condition that has to be fulfilled to determine a dependency between the two tasks
     */
    NBase TaskSynchronizations::match_dependencies(Node* source, Node* target)
    {
        NBase condition;

        // 1.- Collect the variables in the dependency clauses
        Nodecl::List source_environ = source->get_graph_related_ast().as<Nodecl::OpenMP::Task>().get_environment().as<Nodecl::List>();
        Nodecl::List target_environ = target->get_graph_related_ast().as<Nodecl::OpenMP::Task>().get_environment().as<Nodecl::List>();
        // 1.1.- Get in dependencies on one side and out and inout dependencies on the other side
        //       - out|inout will be matched with in|out|inout in the target
        //       - in will be matched with out|inout dependencies
        ObjectList<NBase> source_in_deps = gather_dependences(
            source_environ,
            &Nodecl::OpenMP::DepIn::get_exprs);
        ObjectList<NBase> source_out_deps = gather_dependences(
            source_environ,
            &Nodecl::OpenMP::DepOut::get_exprs);
        ObjectList<NBase> source_inout_deps = gather_dependences(
            source_environ,
            &Nodecl::OpenMP::DepInout::get_exprs);
        ObjectList<NBase> source_all_out_deps = append_two_lists(source_out_deps, source_inout_deps);
        // 1.2.- Get all in, out and inout dependencies
        ObjectList<NBase> target_in_deps = gather_dependences(
            target_environ,
            &Nodecl::OpenMP::DepIn::get_exprs);
        ObjectList<NBase> target_out_deps = gather_dependences(
            target_environ,
            &Nodecl::OpenMP::DepOut::get_exprs);
        ObjectList<NBase> target_inout_deps = gather_dependences(
            target_environ,
            &Nodecl::OpenMP::DepInout::get_exprs);
        ObjectList<NBase> target_deps =
            append_two_lists(target_inout_deps,
                             append_two_lists(target_in_deps, target_out_deps));
        ObjectList<NBase> target_all_out_deps = append_two_lists(target_out_deps, target_inout_deps);

        // 2.- Check each pair of dependencies to build the condition and obtain the modification we have to perform in the dependency edge
        // 2.1.- (RAW, WAW) Match source(out, inout) with target(in, out, inout)
        for (ObjectList<NBase>::iterator its = source_all_out_deps.begin(); its != source_all_out_deps.end(); ++its)
        {
            for (ObjectList<NBase>::iterator itt = target_deps.begin(); itt != target_deps.end(); ++itt)
            {
                NBase cond_part;
                match_dependence(source, target, *its, *itt, cond_part);
                if (cond_part.is_null())    // The pair <*its, *itt> does no cause dependency
                    continue;
                if (condition.is_null())
                {
                    condition = cond_part;
                }
                else
                {   // If 'cond_part' is already in 'condition', then we do not have to add it again
                    if (!Nodecl::Utils::nodecl_contains_nodecl_by_structure(condition, cond_part))
                        condition = Nodecl::LogicalOr::make(condition.shallow_copy(), cond_part, cond_part.get_type());
                }
            }
        }
        // 2.2.- (WAR) Match source(in) with target(out, inout)
        for (ObjectList<NBase>::iterator its = source_in_deps.begin(); its != source_in_deps.end(); ++its)
        {
            for (ObjectList<NBase>::iterator itt = target_all_out_deps.begin(); itt != target_all_out_deps.end(); ++itt)
            {
                NBase cond_part;
                match_dependence(source, target, *its, *itt, cond_part);
                if (cond_part.is_null())    // The pair <*its, *itt> does no cause dependency
                    continue;
                if (condition.is_null())
                {
                    condition = cond_part;
                }
                else
                {   // If 'cond_part' is already in 'condition', then we do not have to add it again
                    if (!Nodecl::Utils::nodecl_contains_nodecl_by_structure(condition, cond_part))
                        condition = Nodecl::LogicalOr::make(condition.shallow_copy(), cond_part, cond_part.get_type());
                }
            }
        }

        return condition;
    }

}
}
}
