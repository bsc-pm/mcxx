/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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
#include "cxx-codegen.h"
#include "cxx-process.h"
#include "cxx-utils.h"

#include "tl-expression-reduction.hpp"
#include "tl-loop-analysis.hpp"

namespace TL {
namespace Analysis {

    LoopAnalysis::LoopAnalysis(ExtensibleGraph* graph, Utils::InductionVarsPerNode ivs)
            : _graph(graph), _induction_vars(ivs), _loop_limits()
    {}

    void LoopAnalysis::compute_loop_ranges()
    {
        Node* graph = _graph->get_graph();
        compute_loop_ranges_rec(graph);
        ExtensibleGraph::clear_visits(graph);
    }

    void LoopAnalysis::compute_loop_ranges_rec(Node* n)
    {
        if (n->is_visited())
            return;

        n->set_visited(true);

        if(n->is_graph_node())
        {
            // First compute recursively the inner nodes
            compute_loop_ranges_rec(n->get_graph_entry_node());

            // If the graph is a loop, compute the current ranges
            // For OpenMP::For nodes, the loop ranges have been already computed since they are synthesized in the ForRange nodecl
            if (n->is_loop_node())
            {
                const Utils::InductionVarList& ivs = n->get_induction_variables();

                for (Utils::InductionVarList::const_iterator it = ivs.begin(); it != ivs.end(); ++it)
                {
                    // The lower bound must be in the Reaching Definitions In set
                    // But we only want the reaching definitions coming from dominators (remove the ones coming from back-edges)
                    const NBase& var = (*it)->get_variable();
                    NodeclSet var_rdi;
                    const ObjectList<Edge*>& entries = n->get_entry_edges();
                    for (ObjectList<Edge*>::const_iterator ite = entries.begin(); ite != entries.end(); ++ite)
                    {
                        if ((*ite)->is_back_edge())
                            continue;

                        // skip entry nodes, which do not have reaching definitions info
                        // FIXME It could happen to have more than one parent while traversing Entry nodes
                        //       and also they may come from a back edge
                        //       This will not be a problem when Reaching Definitions assigns RD sets to the Entry/Exit edges
                        Node* source = (*ite)->get_source();
                        if (source->is_entry_node())
                            source = source->get_outer_node()->get_parents()[0];
                        const NodeclMap& rds = source->is_entry_node() ? source->get_outer_node()->get_reaching_definitions_in()
                                                                       : source->get_reaching_definitions_out();
                        std::pair<NodeclMap::const_iterator, NodeclMap::const_iterator> var_rd_pair = rds.equal_range(var);
                        for (NodeclMap::const_iterator it_rdi = var_rd_pair.first; it_rdi != var_rd_pair.second; ++it_rdi)
                            var_rdi.insert(it_rdi->second.first);
                    }

                    if (!var_rdi.empty())
                    {
                        (*it)->set_lb(var_rdi);
                    }
                    else
                    {
                        if(VERBOSE)
                        {
                            WARNING_MESSAGE("Cannot compute the lower bound of the Induction Variable '%s' in node '%d'",
                                            var.prettyprint().c_str(), n->get_id());
                        }
                    }
                }

                // Loop limits
                // It is convenient to use the InductionVar structure, even though the return of the
                // condition parsing might not be an Induction Variable but a variable that defined the limits
                // of the loop

                // The upper bound
                        // Easy case: the condition node contains the upper bound of the induction variable
                Node* condition_node = n->get_condition_node();
                if (condition_node != NULL)
                {
                    const NodeclList& stmts = condition_node->has_statements()
                            ? condition_node->get_statements()
                                    : NodeclList(1, condition_node->get_graph_related_ast());
                    NBase condition_stmt = stmts[0];
                    get_loop_limits(condition_stmt, n->get_id());
                }

                        // The upper bound must be computed depending on the loop limits
                // TODO

            }
        }

        // Compute ranges for the following loops
        const ObjectList<Node*>& children = n->get_children();
        for (ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
            compute_loop_ranges_rec(*it);
    }

    void LoopAnalysis::get_loop_limits(NBase cond, int loop_id)
    {
        Optimizations::ReduceExpressionVisitor v;

        if(cond.is<Nodecl::Symbol>())
        {   // No limits to be computed
        }
        else if(cond.is<Nodecl::LogicalAnd>())
        {
            // Traverse left and right parts
            Nodecl::LogicalAnd cond_ = cond.as<Nodecl::LogicalAnd>();
            get_loop_limits(cond_.get_lhs(), loop_id);
            get_loop_limits(cond_.get_rhs(), loop_id);
        }
        else if(cond.is<Nodecl::LogicalOr>())
        {
            if(VERBOSE)
            {
                WARNING_MESSAGE("Cannot decide the upper bound of the induction variables of loop %d "
                                 "because the condition is a Nodecl::LogicalOr", loop_id);
            }
        }
        else if(cond.is<Nodecl::LogicalNot>())
        {
            if(VERBOSE)
            {
                WARNING_MESSAGE("Cannot decide the upper bound of the induction variables of loop %d "
                                 "because the condition is a Nodecl::LogicalNot", loop_id);
            }
        }
        else if(cond.is<Nodecl::LowerThan>())
        {
            Nodecl::LowerThan cond_ = cond.as<Nodecl::LowerThan>();
            NBase var = cond_.get_lhs();
            NBase var_limit = cond_.get_rhs();

            // The upper bound will be the rhs minus 1
            NBase ub;
            const_value_t* one_const = const_value_get_one(/* bytes */ 4, /* signed*/ 1);
            if(var_limit.is_constant())
                ub = const_value_to_nodecl(const_value_sub(var_limit.get_constant(), one_const));
            else
                ub = Nodecl::Minus::make(var_limit.shallow_copy(), const_value_to_nodecl(one_const), var.get_type());
            v.walk(ub);

            std::pair<Utils::InductionVarsPerNode::iterator, Utils::InductionVarsPerNode::iterator> loop_ivs =
                    _induction_vars.equal_range(loop_id);
            Utils::InductionVar* loop_info_var = get_induction_variable_from_list(
                    Utils::InductionVarsPerNode(loop_ivs.first, loop_ivs.second), var);
            if (loop_info_var != NULL)
            {
                loop_info_var->set_ub(ub);
            }
            else
            {
                loop_info_var = new Utils::InductionVar(var);
                loop_info_var->set_ub(ub);
                _loop_limits.insert(Utils::InductionVarsPerNode::value_type(loop_id, loop_info_var));
            }
        }
        else if(cond.is<Nodecl::LowerOrEqualThan>())
        {
            Nodecl::LowerThan cond_ = cond.as<Nodecl::LowerThan>();
            NBase var = cond_.get_lhs();
            NBase var_limit = cond_.get_rhs();

            std::pair<Utils::InductionVarsPerNode::iterator, Utils::InductionVarsPerNode::iterator> loop_ivs =
                    _induction_vars.equal_range(loop_id);
            Utils::InductionVar* loop_info_var = get_induction_variable_from_list(
                    Utils::InductionVarsPerNode(loop_ivs.first, loop_ivs.second), var);
            if (loop_info_var != NULL)
            {
                loop_info_var->set_ub(var_limit);
            }
            else
            {
                loop_info_var = new Utils::InductionVar(var);
                loop_info_var->set_ub(var_limit);
                _loop_limits.insert(Utils::InductionVarsPerNode::value_type(loop_id, loop_info_var));
            }
        }
        else if(cond.is<Nodecl::GreaterThan>())
        {
            Nodecl::GreaterThan cond_ = cond.as<Nodecl::GreaterThan>();
            NBase var = cond_.get_lhs();
            NBase var_limit = cond_.get_rhs();

            // This is not the UB, is the LB: the lower bound will be the rhs plus 1
            NBase lb;
            const_value_t* one_const = const_value_get_one(/*bytes*/ 4, /*signed*/ 1);
            if(var_limit.is_constant())
                lb = const_value_to_nodecl(const_value_add(var_limit.get_constant(), one_const));
            else
                lb = Nodecl::Minus::make(var_limit.shallow_copy(), const_value_to_nodecl(one_const), var.get_type());
            v.walk(lb);

            std::pair<Utils::InductionVarsPerNode::iterator, Utils::InductionVarsPerNode::iterator> loop_ivs =
                    _induction_vars.equal_range(loop_id);
            Utils::InductionVar* loop_info_var = get_induction_variable_from_list(
                    Utils::InductionVarsPerNode(loop_ivs.first, loop_ivs.second), var);
            if (loop_info_var != NULL)
            {
                loop_info_var->set_ub(loop_info_var->get_lb());
                loop_info_var->set_lb(lb);
            }
            else
            {
                loop_info_var = new Utils::InductionVar(var);
                loop_info_var->set_lb(lb);
                _loop_limits.insert(Utils::InductionVarsPerNode::value_type(loop_id, loop_info_var));
            }
        }
        else if(cond.is<Nodecl::GreaterOrEqualThan>())
        {
            Nodecl::GreaterThan cond_ = cond.as<Nodecl::GreaterThan>();
            NBase var = cond_.get_lhs();
            NBase var_limit = cond_.get_rhs();

            std::pair<Utils::InductionVarsPerNode::iterator, Utils::InductionVarsPerNode::iterator> loop_ivs =
                    _induction_vars.equal_range(loop_id);
            Utils::InductionVar* loop_info_var = get_induction_variable_from_list(
                    Utils::InductionVarsPerNode(loop_ivs.first, loop_ivs.second), var);
            if(loop_info_var != NULL)
            {
                loop_info_var->set_ub(loop_info_var->get_lb());
                loop_info_var->set_lb(var_limit);
            }
            else
            {
                loop_info_var = new Utils::InductionVar(var);
                loop_info_var->set_lb(var_limit);
                _loop_limits.insert(Utils::InductionVarsPerNode::value_type(loop_id, loop_info_var));
            }
        }
        else if(cond.is<Nodecl::Different>())
        {
            if(VERBOSE)
            {
                WARNING_MESSAGE("Cannot decide the upper bound of the induction variables of loop %d "
                                 "because the condition is a Nodecl::Different", loop_id);
            }
        }
        else if(cond.is<Nodecl::Equal>())
        {
            if(VERBOSE)
            {
                WARNING_MESSAGE("Cannot decide the upper bound of the induction variables of loop %d "
                                 "because the condition is a Nodecl::Equal", loop_id);
            }
        }
        else
        {   // TODO Complex expression in the condition node may contain an UB or LB of the induction variable
        }
    }

}
}
