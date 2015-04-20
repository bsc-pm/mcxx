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
                    points_of_sync[alive_tasks_it->node].insert(std::make_pair(current_sync_point, sync_kind));

                    if (task_sync_rel == tribool::yes)
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
                if(!source_is_subobject || !target_is_subobject)
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
            else if (it->is<Nodecl::OpenMP::Commutative>())
                source_dep_commutative = *it;
        }

        // Target (taskwait)
        ERROR_CONDITION(target->get_type() != __OmpWaitonDeps, "Invalid node", 0);
        NodeclList task_node_target_stmts = target->get_statements();
        ERROR_CONDITION(task_node_target_stmts.size() != 1, "Invalid list of statements", 0);
        NBase taskwait_node_target = task_node_target_stmts[0];
        ERROR_CONDITION(taskwait_node_target.is_null(), "Invalid target task tree", 0);
        ERROR_CONDITION(!taskwait_node_target.is<Nodecl::OpenMP::WaitOnDependences>(),
                "Expecting an OpenMP::WaitOnDependences target node here got a %s",
                ast_print_node_type(taskwait_node_target.get_kind()));

        Nodecl::List task_target_env = taskwait_node_target
            .as<Nodecl::OpenMP::WaitOnDependences>()
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
        std::cerr << "CHECKING DEPENDENCES STATICALLY " << source->get_id() << " -> " << target->get_id() << std::endl;
#endif

        // TL::NodeclList source_statements = source->get_statements();
        // ERROR_CONDITION(source_statements.empty(), "Invalid source statement set", 0);
        NBase task_node_source = source->get_graph_related_ast();
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

        // FIXME: These should be lists. Otherwise, when we have more than one clause of the same type, we will end up with the last one
        // FIXME: Extend support for concurrent dependencies
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
            else if (it->is<Nodecl::OpenMP::Commutative>())
                source_dep_commutative = *it;
        }

        // TL::NodeclList target_statements = target->get_statements();
        // ERROR_CONDITION(target_statements.empty(), "Invalid target statement set", 0);
        NBase task_node_target = target->get_graph_related_ast();
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

        NBase target_dep_in;
        NBase target_dep_out;
        NBase target_dep_inout;
        NBase target_dep_commutative;
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
            else if (it->is<Nodecl::OpenMP::Commutative>())
                target_dep_commutative = *it;
        }

        tribool may_have_dep = tribool::no;

        // DRY
        Nodecl::NodeclBase sources[] = { source_dep_in, source_dep_inout, source_dep_out, source_dep_commutative };
        int num_sources = sizeof(sources)/sizeof(sources[0]);
        Nodecl::NodeclBase targets[] = { target_dep_in, target_dep_inout, target_dep_out, target_dep_commutative };
        int num_targets = sizeof(targets)/sizeof(targets[0]);

        for (int n_source = 0; n_source < num_sources; n_source++)
        {
            if (sources[n_source].is_null())
                continue;
            
            for (int n_target = 0; n_target < num_targets; n_target++)
            {
                if (targets[n_target].is_null())
                    continue;

                may_have_dep = may_have_dep || 
                    // At least one of the dependences is not only an input
                    ((!is_only_input_dependence(sources[n_source])
                      || !is_only_input_dependence(targets[n_target]))
                     // Note we (ab)use the fact that DepIn/DepOut/DepInOut all have the
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
                tribool task_sync_rel;
                if (alive_tasks_it->domain == current_domain_id)
                {
                    task_sync_rel = compute_task_sync_relationship(alive_tasks_it->node, task);
                }
                else
                {
                    task_sync_rel = tribool::no;
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

    bool IsOmpssEnabled = false;
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

        Node* exit = root->get_graph_exit_node();
        if (exit->get_live_in_tasks().empty())
            return;
        Node* post_sync = _graph->create_unconnected_node(__OmpVirtualTaskSync, NBase::null());
        _graph->set_post_sync(post_sync);
        for (AliveTaskSet::iterator it = exit->get_live_in_tasks().begin();
                it != exit->get_live_in_tasks().end();
                it++)
        {
            Edge* edge = _graph->connect_nodes(it->node, post_sync, __Always,
                    NBase::null(), /*is task edge*/ true);

            edge->set_sync_kind(__Post);
            const char* s = edge->get_sync_kind_as_string();
            edge->set_label(Nodecl::StringLiteral::make(Type(get_literal_string_type(strlen(s)+1, get_char_type())),
                                                        const_value_make_string(s, strlen(s))));
        }
    }

    // ************************* END class implementing task PCFG synchronization ************************* //
    // **************************************************************************************************** //



    // **************************************************************************************************** //
    // *************************** Class implementing task concurrency analysis *************************** //

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
            if (child->is_omp_task_node() &&
                (visited_tasks.find(child)==visited_tasks.end()) &&     // Avoid cycles
                ((*it)->get_sync_kind() != __Maybe))              // This path will be taken for sure
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
        if(current->is_visited_aux() || (current == last))
            return;

        current->set_visited_aux(true);

        if(current->is_exit_node())
            return;

        if(current->is_graph_node())
        {
            // Add inner tasks recursively, if exist
            collect_tasks_between_nodes(current->get_graph_entry_node(), last, skip, result);

            // Add current node if it is a task
            if (current->is_omp_task_node() && (current != skip) && !tasks_are_synchronized(current, skip))
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
            if ((*it) != task)
            {
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
                            buff.push(*itc);
                        
                        while(!buff.empty())
                        {
                            Node* current = buff.front();
                            buff.pop();
                            
                            for (ObjectList<Node*>::const_iterator itc = children.begin(); itc != children.end(); ++itc)
                            {
                                if (ExtensibleGraph::node_is_ancestor_of_node(*itc, current))
                                {
                                    concurrent_tasks.insert(*it);
                                    goto task_synchronized;
                                }
                                else if ((*itc)->is_omp_task_node())
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
    }

    static bool task_synchronizes_in_all_paths_within_loop(Node* current, Node* task, Node* loop)
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
        while(outer != NULL)
        {
            if(outer->is_loop_node())
                loop = outer;
            outer = outer->get_outer_node();
        }
        return loop;
    }
    
    //! Returns the most outer parallel node of a given \p task, or NULL, if no parallel exists
    UNUSED_FUNCTION Node* get_most_outer_parallel(Node* task)
    {
        Node* parallel = NULL;
        Node* outer = task->get_outer_node();
        while(outer != NULL)
        {
            if(outer->is_omp_parallel_node())
                parallel = outer;
            outer = outer->get_outer_node();
        }
        return parallel;
    }
    
    //! Returns the immediately inner parallel node of a given node \p n, 
    //! before reaching an inner \p task node
    UNUSED_FUNCTION Node* get_inner_parallel(Node* parallel, Node* task)
    {
        Node* inner_parallel = NULL;
        Node* outer = task->get_outer_node();
        while(outer != NULL)
        {
            if(outer == parallel)
                break;
            if(outer->is_omp_parallel_node())
                inner_parallel = outer;
            outer = outer->get_outer_node();
        }
        return inner_parallel;
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

    // ************************* END class implementing task concurrency analysis ************************* //
    // **************************************************************************************************** //
}
}
}
