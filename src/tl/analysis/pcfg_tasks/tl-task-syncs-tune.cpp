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
#include "tl-task-syncs-tune.hpp"

namespace TL {
namespace Analysis {
namespace TaskAnalysis{

namespace {

    enum SyncModification
    {
        None            = 0,
        Keep            = 1u << 0,
        MaybeToStatic   = 1u << 1,
        Remove          = 1u << 2
    };

    inline SyncModification operator|(SyncModification a, SyncModification b)
    {
        return static_cast<SyncModification>(static_cast<int>(a) | static_cast<int>(b));
    }

    void compute_condition_for_unmatched_values(
            const NBase& n,
            const NBase& m,
            NBase& condition)
    {
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
            }
        }
        
        // Rebuild the condition composing the old condition and the new computed part
        if(condition.is_null())
            condition = cond_part;
        else
            condition = Nodecl::LogicalAnd::make(condition.shallow_copy(), cond_part, condition.get_type());
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
    SyncModification match_const_and_var_values(const NBase& n, const NBase& m,
                                                Node* m_node, NBase& condition)
    {
        SyncModification modification_type = Keep;

        NodeclMap m_reaching_defs_in = m_node->get_reaching_definitions_in();
        NodeclSet m_killed_vars = m_node->get_killed_vars();
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
                    modification_type = match_const_and_var_values(n, m_reach_def, m_node, condition);
                }
                else
                {   // We do not know whether the indexes are equal => compute the condition
                    compute_condition_for_unmatched_values(n, m, condition);
                }
            }
        }
        else
        {   // We do not know whether the indexes are equal => compute the condition
            compute_condition_for_unmatched_values(n, m, condition);
        }

        return modification_type;
    }

    SyncModification match_variable_values(const NBase& n, const NBase& m,
                                            Node* n_node, Node* m_node, NBase& condition)
    {
        SyncModification modification_type = Keep;

        NodeclMap n_reaching_defs_in = n_node->get_reaching_definitions_in();
        NodeclSet n_killed_vars = n_node->get_killed_vars();
        NodeclMap m_reaching_defs_in = m_node->get_reaching_definitions_in();
        NodeclSet m_killed_vars = m_node->get_killed_vars();

        if((n_reaching_defs_in.count(n) == 1) && (n_killed_vars.find(n) == n_killed_vars.end()) &&
            (m_reaching_defs_in.count(m) == 1) && (m_killed_vars.find(m) == m_killed_vars.end()))
        {
            NBase n_reach_def = n_reaching_defs_in.find(n)->second.first;
            NBase m_reach_def = m_reaching_defs_in.find(m)->second.first;

            if(n_reach_def.is_constant())
            {   // n definition is constant
                if(m_reach_def.is_constant())
                {   // m definition is constant
                    modification_type = match_constant_values(n_reach_def, m_reach_def, condition);
                }
                else
                {   // m is not constant | Try to compute equality from the reaching definition of m
                    modification_type = match_const_and_var_values(n_reach_def, m_reach_def, m_node, condition);
                }
            }
            else
            {
                if(m_reach_def.is_constant())
                {   // n is not constant | Try to compute equality from the reaching definition of n
                    modification_type = match_const_and_var_values(m_reach_def, n_reach_def, m_node, condition);
                }
                else
                {
                    if(n_reach_def.is<Nodecl::Symbol>() && m_reach_def.is<Nodecl::Symbol>())
                    {   // n, m reaching definitions are symbols | Try to compute the equality from its reaching definitions
                        modification_type = match_variable_values(n_reach_def, m_reach_def, n_node, m_node, condition);
                    }
                    else
                    {
                        compute_condition_for_unmatched_values(n, m, condition);
                    }
                }
            }
        }
        else
        {   // We do not know whether the indexes are equal => compute the condition
            compute_condition_for_unmatched_values(n, m, condition);
        }

        return modification_type;
    }

    bool variable_is_modified_between_nodes_rec(Node* source, Node* target, const NBase& var, bool& is_modified)
    {
        if(source == target)
            return true;
        
        bool target_found = false;
        if(!source->is_visited_aux())
        {
            source->set_visited_aux(true);
            
            //Call recursively first, so we ensure we find the node where it is modified before we know it is modified
            if(source->is_graph_node())
                target_found = variable_is_modified_between_nodes_rec(source->get_graph_entry_node(), target, var, is_modified);
            
            // Just check whether the @source node defines the variable if
            // we have not found before another node that defines it
            // and if we are not in a graph node, because we already checked it inner nodes
            if(!is_modified && !source->is_graph_node())
            {
                NodeclSet killed_vars;
                if(source->is_omp_task_creation_node())
                {   // Variables from non-task children nodes do not count here
                    Node* created_task = ExtensibleGraph::get_task_from_task_creation(source);
                    ERROR_CONDITION(created_task==NULL,
                                    "Task created by task creation node %d not found.\n",
                                    source->get_id());
                    killed_vars = created_task->get_killed_vars();
                }
                else
                {
                    killed_vars = source->get_killed_vars();
                }
                if(killed_vars.find(var) != killed_vars.end())
                {
                    NodeclMap reach_defs_out = source->get_reaching_definitions_out();
                    NodeclMap::iterator var_out_definition = reach_defs_out.find(var);
                    ERROR_CONDITION(var_out_definition==reach_defs_out.end(),
                                    "No RD_OUT found in node %d for variable %s, but it is in the list of KILLED variables.\n",
                                    source->get_id(), var.prettyprint().c_str());
                    if(!Nodecl::Utils::structurally_equal_nodecls(var, var_out_definition->second.first))
                    {   // Avoid reporting a modification for expressions like var = var;
                        is_modified = true;
                    }
                }
            }
            
            // Keep traversing the graph to check that this path drives to @target
            const ObjectList<Node*>& children = (source->is_exit_node() ? source->get_outer_node()->get_children() 
                                                                        : source->get_children());
            for(ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
            {
                if(!(*it)->is_omp_task_node())
                    target_found = target_found || variable_is_modified_between_nodes_rec(*it, target, var, is_modified);
            }
        }
        return target_found;
    }
    
    bool variable_is_modified_between_nodes(Node* source, Node* target, const NBase& var)
    {
        bool is_modified = false;
        bool target_found = false;
        if(source == target)
        {   // This may happen when a task has dependencies with itself because it is enclosed in an iterative construct
            // In this case we have to call recursively with the children of @source
            const ObjectList<Node*>& source_children = source->get_children();
            for(ObjectList<Node*>::const_iterator it = source_children.begin(); it != source_children.end(); ++it)
            {
                if(!(*it)->is_omp_task_node())
                {
                    target_found = target_found || variable_is_modified_between_nodes_rec(*it, target, var, is_modified);
                    ExtensibleGraph::clear_visits_aux(*it);
                    ERROR_CONDITION(!target_found,
                                    "Unable to find path between %d and %d.\n",
                                    (*it)->get_id(), target->get_id());
                }
            }
        }
        else
        {
            target_found = variable_is_modified_between_nodes_rec(source, target, var, is_modified);
            ExtensibleGraph::clear_visits_aux(source);
            ERROR_CONDITION(!target_found,
                            "Unable to find path between %d and %d.\n",
                            source->get_id(), target->get_id());
        }
        return is_modified;
    }

    SyncModification match_array_subscripts(Node* source, Node* target,
                                             const Nodecl::ArraySubscript& a, const Nodecl::ArraySubscript& b,
                                             NBase& condition)
    {
        SyncModification modification_type = Keep;
        Nodecl::List source_subscripts = a.get_subscripts().as<Nodecl::List>();
        Nodecl::List target_subscripts = b.get_subscripts().as<Nodecl::List>();
        Nodecl::List::iterator its = source_subscripts.begin();
        Nodecl::List::iterator itt = target_subscripts.begin();
        for(; (its != source_subscripts.end()) && (modification_type != Remove); ++its, ++itt)
        {
            if(its->is_constant())
            {   // source[c1]
                if(itt->is_constant())
                {   // target[c2]
                    modification_type = match_constant_values(*its, *itt, condition);
                }
                else
                {   // target[v2]
                    modification_type = match_const_and_var_values(*its, *itt, target, condition);
                }
            }
            else
            {   // source[v1]
                if(itt->is_constant())
                {   // target[c2]
                    modification_type = match_const_and_var_values(*itt, *its, source, condition);
                }
                else
                {   // targt_v2
                    // TODO Can we do something here?
                    if(Nodecl::Utils::structurally_equal_nodecls(*its, *itt, /*skip_conversions*/ true))
                    {
                        const NBase& var = *its;
                        // Case 1: the two variables are the same
                        //         the two variables are firstprivate for each task
                        //         => check the values captured between the two task creations
                        TL::Analysis::PCFGPragmaInfo source_pragma_info = source->get_pragma_node_info();
                        TL::Analysis::PCFGPragmaInfo target_pragma_info = target->get_pragma_node_info();
                        if (source_pragma_info.has_clause(NODECL_OPEN_M_P_FIRSTPRIVATE) &&
                            target_pragma_info.has_clause(NODECL_OPEN_M_P_FIRSTPRIVATE))
                        {
                            // Get the list of Firstprivate variables of both source and target
                            Nodecl::List source_fp_vars_list =
                                    source_pragma_info.get_clause(NODECL_OPEN_M_P_FIRSTPRIVATE).as<Nodecl::OpenMP::Firstprivate>().get_symbols().as<Nodecl::List>();
                            Nodecl::List target_fp_vars_list =
                                    target_pragma_info.get_clause(NODECL_OPEN_M_P_FIRSTPRIVATE).as<Nodecl::OpenMP::Firstprivate>().get_symbols().as<Nodecl::List>();
                            // Transform it into a set to use the nodecl structural comparator
                            NodeclSet source_fp_vars_set(source_fp_vars_list.begin(), source_fp_vars_list.end());
                            NodeclSet target_fp_vars_set(target_fp_vars_list.begin(), target_fp_vars_list.end());
                            // Check whether the variable is in both sets
                            if ((source_fp_vars_set.find(var)!=source_fp_vars_set.end()) &&
                                (target_fp_vars_set.find(var)!=target_fp_vars_set.end()))
                            {
                                Node* source_tc = ExtensibleGraph::get_task_creation_from_task(source);
                                Node* target_tc = ExtensibleGraph::get_task_creation_from_task(target);
                                if(!variable_is_modified_between_nodes(source_tc, target_tc, var))
                                {   // The variable hasn't changed => we are sure there is a dependency
                                    modification_type = MaybeToStatic;
                                }
                                else
                                {   // The variable has changed => we are sure there is no dependency
                                    modification_type = Remove;
                                }
                                goto match_array_subscripts_end;
                            }
                        }
                    }
                    
                    compute_condition_for_unmatched_values(*itt, *its, condition);
                }
            }
        }

match_array_subscripts_end:
        return modification_type;
    }

    SyncModification match_dependence(Node* source, Node* target,
                                       const NBase& src_dep, const NBase& tgt_dep,
                                       NBase& condition)
    {
        SyncModification modification_type = Keep;
        
        // Skip Conversion nodes
        if(src_dep.is<Nodecl::Conversion>())
            match_dependence(source, target, src_dep.as<Nodecl::Conversion>().get_nest(), tgt_dep, condition);
        if(tgt_dep.is<Nodecl::Conversion>())
            match_dependence(source, target, src_dep, tgt_dep.as<Nodecl::Conversion>().get_nest(), condition);

        // Skip shaping nodes
        if(src_dep.is<Nodecl::Shaping>())
            match_dependence(source, target, src_dep.as<Nodecl::Shaping>().get_postfix(), tgt_dep, condition);
        if(tgt_dep.is<Nodecl::Shaping>())
            match_dependence(source, target, src_dep, tgt_dep.as<Nodecl::Shaping>().get_postfix(), condition);

        // Compare the two dependencies
        if(src_dep.is<Nodecl::Symbol>())
        {
            if(tgt_dep.is<Nodecl::Symbol>())
            {
                if(Nodecl::Utils::structurally_equal_nodecls(src_dep, tgt_dep))
                    modification_type = MaybeToStatic;
                else
                    modification_type = Remove;
            }
            else
                modification_type = Remove;
        }
        else if(src_dep.is<Nodecl::Dereference>())
        {   // TODO Alias analysis needed for further information here
        }
        else if(src_dep.is<Nodecl::ClassMemberAccess>())
        {
            if(tgt_dep.is<Nodecl::ClassMemberAccess>())
            {
                Nodecl::ClassMemberAccess src_dep_ = src_dep.as<Nodecl::ClassMemberAccess>();
                Nodecl::ClassMemberAccess tgt_dep_ = tgt_dep.as<Nodecl::ClassMemberAccess>();
                if(Nodecl::Utils::structurally_equal_nodecls(src_dep_.get_lhs(), tgt_dep_.get_lhs()))
                    modification_type = match_dependence(source, target,
                                                          src_dep_.get_member(), src_dep_.get_member(),
                                                          condition);
                else
                    modification_type = Remove;
            }
            else
                modification_type = Remove;
        }
        else if(src_dep.is<Nodecl::ArraySubscript>())
        {
            if(tgt_dep.is<Nodecl::ArraySubscript>())
            {
                modification_type = match_array_subscripts(source, target, src_dep.as<Nodecl::ArraySubscript>(),
                                                            tgt_dep.as<Nodecl::ArraySubscript>(), condition);
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
        
        typedef std::pair<ObjectList<NBase>, ObjectList<NBase> > nodecl_object_list_pair;

        // 1.- Collect the variables in the dependency clauses
        Nodecl::List source_environ = source->get_graph_related_ast().as<Nodecl::OpenMP::Task>().get_environment().as<Nodecl::List>();
        Nodecl::List target_environ = target->get_graph_related_ast().as<Nodecl::OpenMP::Task>().get_environment().as<Nodecl::List>();
        // 1.1.- Get in dependencies on one side and out and inout dependencies on the other side
        //       - out|inout will be matched with in|out|inout in the target
        //       - in will be matched with out|inout dependencies
        ObjectList<NBase> source_in_deps = source_environ.find_all<Nodecl::OpenMP::DepIn>()
                .map(functor(&Nodecl::OpenMP::DepIn::get_in_deps))                  // ObjectList<NBase>
                .map(functor(&NBase::as<Nodecl::List>))                             // ObjectList<Nodecl::List>
                .map(functor(&Nodecl::List::to_object_list))                        // ObjectList<ObjectList<NBase> >
                .reduction(functor(append_two_lists<NBase>))                        // ObjectList<NBase>
                ;
        ObjectList<NBase> source_out_deps = source_environ.find_all<Nodecl::OpenMP::DepOut>()
                .map(functor(&Nodecl::OpenMP::DepOut::get_out_deps))                // ObjectList<NBase>
                .map(functor(&NBase::as<Nodecl::List>))                             // ObjectList<Nodecl::List>
                .map(functor(&Nodecl::List::to_object_list))                        // ObjectList<ObjectList<NBase> >
                .reduction(functor(append_two_lists<NBase>))                        // ObjectList<NBase>
                ;
        ObjectList<NBase> source_inout_deps = source_environ.find_all<Nodecl::OpenMP::DepInout>()
                .map(functor(&Nodecl::OpenMP::DepInout::get_inout_deps))            // ObjectList<NBase>
                .map(functor(&NBase::as<Nodecl::List>))                             // ObjectList<Nodecl::List>
                .map(functor(&Nodecl::List::to_object_list))                        // ObjectList<ObjectList<NBase> >
                .reduction(functor(append_two_lists<NBase>))                        // ObjectList<NBase>
                ;
        ObjectList<NBase> source_all_out_deps = append_two_lists(nodecl_object_list_pair(source_out_deps, source_inout_deps));
        // 1.2.- Get all in, out and inout dependencies
        ObjectList<NBase> target_in_deps = target_environ.find_all<Nodecl::OpenMP::DepIn>()
                .map(functor(&Nodecl::OpenMP::DepIn::get_in_deps))                  // ObjectList<NBase>
                .map(functor(&NBase::as<Nodecl::List>))                             // ObjectList<Nodecl::List>
                .map(functor(&Nodecl::List::to_object_list))                        // ObjectList<ObjectList<NBase> >
                .reduction(functor(append_two_lists<NBase>))                        // ObjectList<NBase>
                ;
        ObjectList<NBase> target_out_deps = target_environ.find_all<Nodecl::OpenMP::DepOut>()
                .map(functor(&Nodecl::OpenMP::DepOut::get_out_deps))                // ObjectList<NBase>
                .map(functor(&NBase::as<Nodecl::List>))                             // ObjectList<Nodecl::List>
                .map(functor(&Nodecl::List::to_object_list))                        // ObjectList<ObjectList<NBase> >
                .reduction(functor(append_two_lists<NBase>))                        // ObjectList<NBase>
                ;
        ObjectList<NBase> target_inout_deps = target_environ.find_all<Nodecl::OpenMP::DepInout>()
                .map(functor(&Nodecl::OpenMP::DepInout::get_inout_deps))            // ObjectList<NBase>
                .map(functor(&NBase::as<Nodecl::List>))                             // ObjectList<Nodecl::List>
                .map(functor(&Nodecl::List::to_object_list))                        // ObjectList<ObjectList<NBase> >
                .reduction(functor(append_two_lists<NBase>))                        // ObjectList<NBase>
                ;
        ObjectList<NBase> target_deps =
                append_two_lists(nodecl_object_list_pair(target_inout_deps,
                                                          append_two_lists(nodecl_object_list_pair(target_in_deps, target_out_deps))));
        ObjectList<NBase> target_all_out_deps = append_two_lists(nodecl_object_list_pair(target_out_deps, target_inout_deps));

        // 2.- Check each pair of dependencies to build the condition and obtain the modification we have to perform in the dependency edge
        SyncModification modification_type = None;
        // 2.1.- Match source(out, inout) with target(in, out, inout)
        for(ObjectList<NBase>::iterator its = source_all_out_deps.begin(); its != source_all_out_deps.end(); ++its)
            for(ObjectList<NBase>::iterator itt = target_deps.begin(); itt != target_deps.end(); ++itt)
                modification_type = modification_type | match_dependence(source, target, *its, *itt, condition);
        // 2.1.- Match source(in) with target(out, inout)
        for(ObjectList<NBase>::iterator its = source_in_deps.begin(); its != source_in_deps.end(); ++its)
            for(ObjectList<NBase>::iterator itt = target_all_out_deps.begin(); itt != target_all_out_deps.end(); ++itt)
                modification_type = modification_type | match_dependence(source, target, *its, *itt, condition);

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
            // Remove any other "strict" synchronization, since now it is synchronized here for sure
            ObjectList<Edge*> sexits = source->get_exit_edges();
            for(ObjectList<Edge*>::iterator it = sexits.begin(); it != sexits.end(); ++it)
            {
                Node* tmp_target = (*it)->get_target();
                if((tmp_target != target) &&
                    ((*it)->get_label_as_string() == "strict"))
                {
                    if(VERBOSE)
                        DEBUG_MESSAGE("Removing unnecessary strict edge between %d and %d", source->get_id(), target->get_id());
                    _pcfg->disconnect_nodes(source, tmp_target);
                    _pcfg->remove_next_synchronization(source, tmp_target);
                }
            }
        }
        else if(modification_type & Remove)
        {   // 3.3.- Case 3: We can remove the dependency edge
            if(VERBOSE)
                DEBUG_MESSAGE("Dependency between %d and %d is being removed", source->get_id(), target->get_id());
            _pcfg->disconnect_nodes(source, target);
            _pcfg->remove_next_synchronization(source, target);
            
        }

        return condition;
    }

}
}
}
