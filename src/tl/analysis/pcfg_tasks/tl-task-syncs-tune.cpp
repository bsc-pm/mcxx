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
#include "tl-task-syncs-tune.hpp"
#include "tl-task-syncs-utils.hpp"

namespace TL {
namespace Analysis {
namespace TaskAnalysis{

namespace {

    enum SyncModification
    {
        None            = 0,        // 0
        Keep            = 1u << 0,  // 1
        MaybeToStatic   = 1u << 1,  // 2
        Remove          = 1u << 2   // 4
    };

    inline SyncModification operator|(SyncModification a, SyncModification b)
    {
        return static_cast<SyncModification>(static_cast<int>(a) | static_cast<int>(b));
    }

    SyncModification compute_condition_for_unmatched_values(
            Node* n_node, Node* m_node,
            const NBase& n, const NBase& m,
            NBase& condition)
    {
        SyncModification modification_type = Keep;

        // Get the condition for the two values
        NBase cond_part;
        Type t = n.get_type();
        if(n.is<Nodecl::Range>())
        {
            if(m.is<Nodecl::Range>())
            {   // n=[lb1, ub1], m=[lb2, ub2]
                cond_part = Nodecl::Different::make(
                    Nodecl::Analysis::RangeIntersection::make(n.shallow_copy(), m.shallow_copy(), t),
                    Nodecl::Analysis::EmptyRange::make(),
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
            if(m.is<Nodecl::Range>())
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
                // Although unknown, they might have the same value
                if (Nodecl::Utils::structurally_equal_nodecls(n, m, /*skip_conversions*/true) &&
                    data_reference_is_modified_between_tasks(n_node, m_node, n))
                {
                    modification_type = Remove;
                    cond_part = Nodecl::NodeclBase::null();
                }
                else
                {
                    cond_part = Nodecl::Equal::make(n.shallow_copy(), m.shallow_copy(), n.get_type());
                }
            }
        }
        
        // Rebuild the condition composing the old condition and the new computed part
        if(condition.is_null())
            condition = cond_part;
        else if (!cond_part.is_null())
            condition = Nodecl::LogicalAnd::make(condition.shallow_copy(), cond_part, condition.get_type());

        return modification_type;
    }

    SyncModification match_constant_values (
            const NBase& n,
            const NBase& m,
            NBase& condition)
    {
        if(const_value_is_zero(const_value_sub(n.get_constant(), m.get_constant())))
        {   // n == m | The two indexes are equal!
            return  MaybeToStatic;
        }
        else
        {   // n != m | The accessed indexes are different => we can remove the dependency
            return Remove;
        }
    }

    // Restriction: #n must be a constant nodecl and # m a non-constant nodecl
    SyncModification match_const_and_var_values(
            Node* n_node, Node* m_node,
            const NBase& n, const NBase& m,
            NBase& condition)
    {
        SyncModification modification_type = Keep;

        const NodeclMap& m_reaching_defs_in = m_node->get_reaching_definitions_in();
        const NodeclSet& m_killed_vars = m_node->get_killed_vars();
        if((m_reaching_defs_in.count(m) == 1) && (m_killed_vars.find(m) == m_killed_vars.end()))
        {   // There is a unique reaching definition of the subscript and it is not defined inside the m_node node
            NBase m_reach_def = m_reaching_defs_in.find(m)->second.first;
            if(m_reach_def.is_constant())
            {   // m definition is constant
                modification_type = match_constant_values(n, m_reach_def, condition);
            }
            else
            {
                if(m_reach_def.is<Nodecl::Symbol>())
                {
                    modification_type = match_const_and_var_values(n_node, m_node, n, m_reach_def, condition);
                }
                else
                {   // We do not know whether the indexes are equal => compute the condition
                    modification_type = compute_condition_for_unmatched_values(n_node, m_node, n, m, condition);
                }
            }
        }
        else
        {   // We do not know whether the indexes are equal => compute the condition
            modification_type = compute_condition_for_unmatched_values(n_node, m_node, n, m, condition);
        }

        return modification_type;
    }

    SyncModification match_array_subscripts(Node* n_node, Node* m_node,
                                            const Nodecl::List& n_subs, const Nodecl::List& m_subs,
                                            NBase& condition)
    {
        SyncModification modification_type = None;
        Nodecl::List::iterator itn = n_subs.begin();
        Nodecl::List::iterator itm = m_subs.begin();
        bool cannot_match = false;
        for(; (itn != n_subs.end()) && !(modification_type & Remove); ++itn, ++itm)
        {
            const Nodecl::NodeclBase& n = *itn;
            const Nodecl::NodeclBase& m = *itm;
            if(cannot_match)
            {
                modification_type = modification_type | compute_condition_for_unmatched_values(n_node, m_node, n, m, condition);
            }
            else if(n.is_constant())
            {   // n_node[c1]
                if(m.is_constant())
                {   // m_node[c2]
                    modification_type = modification_type | match_constant_values(n, m, condition);
                }
                else
                {   // m_node[v2]
                    modification_type = modification_type | match_const_and_var_values(n_node, m_node, n, m, condition);
                }
            }
            else
            {   // n_node[v1]
                if(m.is_constant())
                {   // m_node[c2]
                    modification_type = modification_type | match_const_and_var_values(m_node, n_node, m, n, condition);
                }
                else
                {   // m_node[v2]
                    if (Nodecl::Utils::structurally_equal_nodecls(n, m, /*skip_conversions*/ true)
                            && data_reference_is_modified_between_tasks(n_node, m_node, n))
                    {   // The two variables are the same and the variable has changed => we are sure there is no dependency
                        modification_type = modification_type | Remove;
                        goto match_array_subscripts_end;
                    }
                    else
                    {   // The dependency still exists => compute the condition
                        modification_type = modification_type | compute_condition_for_unmatched_values(n_node, m_node, n, m, condition);
                        cannot_match = true;
                    }
                }
            }
        }
        
        // The variable hasn't changed => we are sure there is a dependency
        if(!cannot_match)
            modification_type = MaybeToStatic;

match_array_subscripts_end:
        return modification_type;
    }

    SyncModification match_dependence(
            Node* n_node, Node* m_node,
            const NBase& n, const NBase& m,
            NBase& condition)
    {
        Nodecl::NodeclBase n_ = n.no_conv();
        Nodecl::NodeclBase m_ = m.no_conv();

        // Skip shaping nodes
        if(n_.is<Nodecl::Shaping>())
            return match_dependence(n_node, m_node, n_.as<Nodecl::Shaping>().get_postfix(), m_, condition);
        if(m_.is<Nodecl::Shaping>())
            return match_dependence(n_node, m_node, n_, m_.as<Nodecl::Shaping>().get_postfix(), condition);

        SyncModification modification_type = Keep;
        
        // Compare the two dependencies
        if(n_.is<Nodecl::Symbol>())
        {
            if(m_.is<Nodecl::Symbol>())
            {
                if(Nodecl::Utils::structurally_equal_nodecls(n_, m_, /*skip_conversions*/true))
                    modification_type = MaybeToStatic;
                else
                    modification_type = Remove;
            }
            else
                modification_type = Remove;
        }
        else if(n_.is<Nodecl::Dereference>())
        {
            // TODO Alias analysis needed for further information here
        }
        else if(n_.is<Nodecl::ClassMemberAccess>())
        {
            if(m_.is<Nodecl::ClassMemberAccess>())
            {
                Nodecl::ClassMemberAccess src_dep_ = n_.as<Nodecl::ClassMemberAccess>();
                Nodecl::ClassMemberAccess tgt_dep_ = m_.as<Nodecl::ClassMemberAccess>();
                if(Nodecl::Utils::structurally_equal_nodecls(src_dep_.get_lhs(), tgt_dep_.get_lhs()))
                    modification_type = match_dependence(n_node, m_node,
                                                         src_dep_.get_member(), src_dep_.get_member(),
                                                         condition);
                else
                    modification_type = Remove;
            }
            else
                modification_type = Remove;
        }
        else if(n_.is<Nodecl::ArraySubscript>())
        {
            if(m_.is<Nodecl::ArraySubscript>())
            {
                modification_type = match_array_subscripts(
                        n_node, m_node,
                        n_.as<Nodecl::ArraySubscript>().get_subscripts().as<Nodecl::List>(),
                        m_.as<Nodecl::ArraySubscript>().get_subscripts().as<Nodecl::List>(),
                        condition);
            }
            else
                modification_type = Remove;
        }

        return modification_type;
    }

}

    TaskSyncTunning::TaskSyncTunning(ExtensibleGraph* pcfg)
        : _pcfg(pcfg)
    {}

    void TaskSyncTunning::tune_task_synchronizations()
    {
        Node* entry = _pcfg->get_graph()->get_graph_entry_node();
        tune_task_synchronizations_rec(entry);
        ExtensibleGraph::clear_visits(entry);
    }

    void TaskSyncTunning::tune_task_synchronizations_rec(Node* current)
    {
        if(!current->is_visited())
        {
            current->set_visited(true);
            // Treat the current node
            if(current->is_graph_node())
            {
                // Treat the inner nodes recursively
                tune_task_synchronizations_rec(current->get_graph_entry_node());
                
                // If the current node is a task, then try to simplify its synchronizations
                if(current->is_omp_task_node())
                {
                    // Tune the synchronizations with its children, if possible
                    ObjectList<Edge*> exits = current->get_exit_edges();
                    for(ObjectList<Edge*>::iterator it = exits.begin(); it != exits.end(); ++it)
                    {
                        if((*it)->get_label_as_string() == "maybe")
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
            for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
            {
                tune_task_synchronizations_rec(*it);
            }
        }
    }

    template <typename DependenceNode>
    static TL::ObjectList<Nodecl::NodeclBase> gather_dependences(
            Nodecl::List list,
            Nodecl::NodeclBase (DependenceNode::*get_dependences)() const)
    {
        ObjectList<NBase> deps = list.find_all<DependenceNode>()
                .map(get_dependences)                // ObjectList<NBase>
                .map(&NBase::as<Nodecl::List>)       // ObjectList<Nodecl::List>
                .map(&Nodecl::List::to_object_list)  // ObjectList<ObjectList<NBase> >
                .reduction(append_two_lists<NBase>); // ObjectList<NBase>

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
    NBase TaskSyncTunning::match_dependencies(Node* source, Node* target)
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
                &Nodecl::OpenMP::DepIn::get_in_deps);

        ObjectList<NBase> source_out_deps = gather_dependences(
                source_environ,
                &Nodecl::OpenMP::DepOut::get_out_deps);

        ObjectList<NBase> source_inout_deps = gather_dependences(
                source_environ,
                &Nodecl::OpenMP::DepInout::get_inout_deps);
            
        ObjectList<NBase> source_all_out_deps = append_two_lists(source_out_deps, source_inout_deps);
        // 1.2.- Get all in, out and inout dependencies
        ObjectList<NBase> target_in_deps = gather_dependences(
                target_environ,
                &Nodecl::OpenMP::DepIn::get_in_deps);
        ObjectList<NBase> target_out_deps = gather_dependences(
                target_environ,
                &Nodecl::OpenMP::DepOut::get_out_deps);
        ObjectList<NBase> target_inout_deps = gather_dependences(
                target_environ,
                &Nodecl::OpenMP::DepInout::get_inout_deps);

        ObjectList<NBase> target_deps =
            append_two_lists(target_inout_deps,
                    append_two_lists(target_in_deps, target_out_deps));
        ObjectList<NBase> target_all_out_deps = append_two_lists(target_out_deps, target_inout_deps);

        // 2.- Check each pair of dependencies to build the condition and obtain the modification we have to perform in the dependency edge
        SyncModification modification_type = None;
        // 2.1.- Match source(out, inout) with target(in, out, inout)
        for(ObjectList<NBase>::iterator its = source_all_out_deps.begin(); its != source_all_out_deps.end(); ++its)
            for(ObjectList<NBase>::iterator itt = target_deps.begin(); itt != target_deps.end(); ++itt)
            {
                NBase cond_part;
                modification_type = modification_type | match_dependence(source, target, *its, *itt, cond_part);
                if (cond_part.is_null())    // The pair <*its, *itt> does no cause dependency
                    continue;
                if (condition.is_null())
                    condition = cond_part;
                else
                    condition = Nodecl::LogicalOr::make(condition.shallow_copy(), cond_part, cond_part.get_type());
            }
        // 2.1.- Match source(in) with target(out, inout)
        for(ObjectList<NBase>::iterator its = source_in_deps.begin(); its != source_in_deps.end(); ++its)
            for(ObjectList<NBase>::iterator itt = target_all_out_deps.begin(); itt != target_all_out_deps.end(); ++itt)
            {
                NBase cond_part;
                modification_type = modification_type | match_dependence(source, target, *its, *itt, cond_part);
                if (cond_part.is_null())    // The pair <*its, *itt> does no cause dependency
                    continue;
                if (condition.is_null())
                    condition = cond_part;
                else
                    condition = Nodecl::LogicalOr::make(condition.shallow_copy(), cond_part, cond_part.get_type());
            }

        // 3.- Perform the modifications according to the previous results
        //     The order of these condition is important because for each pair of variables in the dependency clauses
        //     we compute a modification that is joined to the previous ones. So these conditions go from 
        //     the most restrictive to the most relaxed
        if(modification_type & Keep)
        {   // 3.1.- Case 1: we need to keep the dependency as it is because we were not able to simplify it
            //       Nothing to be done here
        }
        else if(modification_type & MaybeToStatic)
        {   // 3.2.- Case 2: we are sure the dependency occurs so we transform it from 'maybe' to 'static'
            if(VERBOSE)
                DEBUG_MESSAGE("Dependency between %d and %d changes from maybe to static", source->get_id(), target->get_id());
            // Transform the type of the edge from "maybe" to "static"
            Edge* e = ExtensibleGraph::get_edge_between_nodes(source, target);
            const char* s = "static";
            e->set_label(Nodecl::StringLiteral::make(Type(get_literal_string_type(strlen(s)+1, get_char_type())),
                                                     const_value_make_string(s, strlen(s))));
            // Remove the target task from the source's list of concurrent tasks
            _pcfg->remove_concurrent_task(source, target);
            // Remove any other "strict" synchronization, since now it is synchronized here for sure
            ObjectList<Edge*> sexits = source->get_exit_edges();
            for(ObjectList<Edge*>::iterator it = sexits.begin(); it != sexits.end(); ++it)
            {
                Node* tmp_target = (*it)->get_target();
                if((tmp_target != target) &&
                    ((*it)->get_label_as_string() == "strict"))
                {
                    if(VERBOSE)
                        DEBUG_MESSAGE("Removing unnecessary strict edge between %d and %d", source->get_id(), tmp_target->get_id());
                    disconnect_tasks(source, tmp_target);
                }
            }
        }
        else if(modification_type & Remove)
        {   // 3.3.- Case 3: We can remove the dependency edge
            if(VERBOSE)
                DEBUG_MESSAGE("Dependency between %d and %d is being removed", source->get_id(), target->get_id());
            disconnect_tasks(source, target);
        }

        return condition;
    }

    void TaskSyncTunning::disconnect_tasks(Node* source, Node* target)
    {
        // Disconnect the two tasks
        _pcfg->disconnect_nodes(source, target);
        
        // Remove the target from the list of "next_synchronizations" of the source
        _pcfg->remove_next_sync_for_tasks(source, target);
    }
    
}
}
}
