/*--------------------------------------------------------------------
 ( C) Copyright 2006-2014 Barcelona Supe*rcomputing Center             *
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

#include <iomanip>

#include "cxx-cexpr.h"
#include "tl-counters.hpp"
#include "tl-analysis-utils.hpp"
#include "tl-expression-reduction.hpp"
#include "tl-task-dependency-graph.hpp"
#include "tl-induction-variables-data.hpp"

namespace TL {
namespace Analysis {

    static unsigned nt = 0;

namespace {
    std::map<FTDGNode*, std::set<ETDGNode*> > ftdg_to_etdg_nodes;
    unsigned task_id = 0;

    std::map<FTDGNode*, LoopInfo*> ftdgnode_to_loop_info;
    std::map<FTDGNode*, unsigned> ftdgnode_to_task_id;

    const_value_t* get_constant(Node* n, NBase st)
    {
        if (st.is_constant())
            return st.get_constant();

        NBase malleable_st = st.shallow_copy();
        NodeclList mem_accesses = Nodecl::Utils::get_all_memory_accesses(st);
        NodeclMap& rd_in = n->get_reaching_definitions_in();
        for (NodeclList::iterator it = mem_accesses.begin(); it != mem_accesses.end(); ++it)
        {
            NBase var = *it;
            ERROR_CONDITION(rd_in.count(var) < 1,
                            "No reaching definition for variable %s in node %d.\n",
                            var.prettyprint().c_str(), n->get_id());
            ERROR_CONDITION(rd_in.count(var) > 1,
                            "More than reaching definition for variable %s in node %d.\n",
                            var.prettyprint().c_str(), n->get_id());

            NodeclMap::iterator var_rd_in_it = rd_in.find(var);
            NBase var_rd_in = var_rd_in_it->second.first.shallow_copy();
            ERROR_CONDITION(var_rd_in.is<Nodecl::Unknown>(),
                            "Unknown reaching definition for variable %s in node %d.\n",
                            var.prettyprint().c_str(), n->get_id());
            const_value_t* var_rd_in_const;
            if (var_rd_in.is_constant())
            {
                var_rd_in_const = var_rd_in.get_constant();
            }
            else
            {
                var_rd_in_const = get_constant(n, var_rd_in);
            }
            NBase var_rd_in_const_n(const_value_to_nodecl(var_rd_in_const));
            Nodecl::Utils::nodecl_replace_nodecl_by_structure(malleable_st, var, var_rd_in_const_n);
        }

        Optimizations::ReduceExpressionVisitor rev;
        rev.walk(malleable_st);
        ERROR_CONDITION(!malleable_st.is_constant(),
                        "Expression %s could not be reduced to a constant.\n",
                        st.prettyprint().c_str());

        return malleable_st.get_constant();
    }

    Nodecl::NodeclBase reduce_to_constant_as_possible(Node* n, NBase st)
    {
        if (st.is_constant())
            return st;

        NBase malleable_st = st.shallow_copy();
        NodeclList mem_accesses = Nodecl::Utils::get_all_memory_accesses(st);
        NodeclMap& rd_in = n->get_reaching_definitions_in();
        for (NodeclList::iterator it = mem_accesses.begin(); it != mem_accesses.end(); ++it)
        {
            NBase var = *it;
            ERROR_CONDITION(rd_in.count(var) < 1,
                            "No reaching definition for variable %s in node %d.\n",
                            var.prettyprint().c_str(), n->get_id());

            // This memory access cannot be reduced at this point
            // because it has more than one possible value
            if (rd_in.count(var) > 1)
                continue;

            NodeclMap::iterator var_rd_in_it = rd_in.find(var);
            NBase var_rd_in = var_rd_in_it->second.first.shallow_copy();
            ERROR_CONDITION(var_rd_in.is<Nodecl::Unknown>(),
                            "Unknown reaching definition for variable %s in node %d.\n",
                            var.prettyprint().c_str(), n->get_id());
            if (!var_rd_in.is_constant())
            {
                var_rd_in = reduce_to_constant_as_possible(n, var_rd_in);
            }
            Nodecl::Utils::nodecl_replace_nodecl_by_structure(malleable_st, var, var_rd_in);
        }

        Optimizations::ReduceExpressionVisitor rev;
        rev.walk(malleable_st);

        return malleable_st;
    }

    class LHSVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
    private:
        std::vector<Nodecl::Symbol> _lhs_vars;

    public:
        LHSVisitor()
            : _lhs_vars()
        {}

        std::vector<Nodecl::Symbol> get_lhs_vars() const
        {
            return _lhs_vars;
        }

        void visit(const Nodecl::Symbol& n)
        {
            _lhs_vars.push_back(n);
        }

        void visit(const Nodecl::Equal& n)
        {
            walk(n.get_lhs());
        }

        void visit(const Nodecl::Different& n)
        {
            walk(n.get_lhs());
        }

        void visit(const Nodecl::LowerThan& n)
        {
            walk(n.get_lhs());
        }

        void visit(const Nodecl::LowerOrEqualThan& n)
        {
            walk(n.get_lhs());
        }

        void visit(const Nodecl::GreaterThan& n)
        {
            walk(n.get_lhs());
        }

        void visit(const Nodecl::GreaterOrEqualThan& n)
        {
            walk(n.get_lhs());
        }
    };

    class RHSVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
    private:
        std::vector<Nodecl::Symbol> _rhs_vars;

    public:
        RHSVisitor()
            : _rhs_vars()
        {}

        std::vector<Nodecl::Symbol> get_rhs_vars() const
        {
            return _rhs_vars;
        }

        void visit(const Nodecl::Symbol& n)
        {
            _rhs_vars.push_back(n);
        }

        void visit(const Nodecl::Equal& n)
        {
            walk(n.get_rhs());
        }

        void visit(const Nodecl::Different& n)
        {
            walk(n.get_rhs());
        }

        void visit(const Nodecl::LowerThan& n)
        {
            walk(n.get_rhs());
        }

        void visit(const Nodecl::LowerOrEqualThan& n)
        {
            walk(n.get_rhs());
        }

        void visit(const Nodecl::GreaterThan& n)
        {
            walk(n.get_rhs());
        }

        void visit(const Nodecl::GreaterOrEqualThan& n)
        {
            walk(n.get_rhs());
        }
    };

    void store_dependency_relevant_vars_rec(
            const Nodecl::Symbol& var, FTDGNode* n, const NodeclMap& reach_defs, Node* ctx,
            /*inout*/std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less>& fixed_relevant_vars,
            /*inout*/std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less>& variable_relevant_vars)
    {
        // Avoid evaluating variables already fixed in a previous context
        if (fixed_relevant_vars.find(var) != fixed_relevant_vars.end())
            return;

        Node* pcfg_n = n->get_pcfg_node();

        NodeclMap::const_iterator var_reach_def_it = reach_defs.find(var);
        ERROR_CONDITION(var_reach_def_it == reach_defs.end(),
                        "No reaching definition found for variable %s in node %d.\n",
                        var.prettyprint().c_str(), pcfg_n->get_id());

        NBase var_reach_def = var_reach_def_it->second.first.shallow_copy();
        // If we are in a loop, some variables will depend on the iteration
        // Otherwise, all variables must be fixed at this moment
        if (ctx->is_loop_node())
        {
            Scope ctx_sc = ctx->get_graph_related_ast().retrieve_context();
            if (var.get_symbol().get_scope().scope_is_enclosed_by(ctx_sc))
            {
                internal_error("Task '%s' is using variable '%s' that is created within the expanded context. This is not supported yet.\n",
                               n->get_pcfg_node()->get_graph_related_ast().get_locus_str().c_str(),
                               var.prettyprint().c_str());
            }

            NodeclSet& ctx_def = ctx->get_killed_vars();
            if (ctx_def.find(var) == ctx_def.end())
            {   // The variable is not modified within the context,
                // insert here the constant value
                ERROR_CONDITION(reach_defs.count(var) > 1,
                                "More than one reaching definition found for variable  %s in node %d.\n",
                                var.prettyprint().c_str(), pcfg_n->get_id());

                fixed_relevant_vars.insert(
                        std::pair<NBase, const_value_t*>(var, get_constant(pcfg_n, var_reach_def)));
            }
            else
            {   // The variable is modified within the context,
                // just insert it, we will compute the value during expansion
                variable_relevant_vars.insert(
                        std::pair<NBase, NBase>(var, var_reach_def));
                // Store here any other variable involved in the definition of 'var'
                ObjectList<NBase> var_def = Nodecl::Utils::get_all_memory_accesses(var_reach_def);
                for (ObjectList<NBase>::iterator itd = var_def.begin();
                        itd != var_def.end(); ++itd)
                {
                    // Do not store again the variable
                    if (!Nodecl::Utils::structurally_equal_nodecls(var, *itd, /*skip conversions*/ true))
                    {
                        ERROR_CONDITION(!itd->is<Nodecl::Symbol>(),
                                        "Non symbol '%s' found while storing relevant vars.\n",
                                        itd->prettyprint().c_str());
                        store_dependency_relevant_vars_rec(
                                itd->as<Nodecl::Symbol>(), n, reach_defs, ctx,
                                fixed_relevant_vars, variable_relevant_vars);
                    }
                }
            }
        }
        else
        {
            if (reach_defs.count(var) == 1)
            {
                fixed_relevant_vars.insert(
                        std::pair<NBase, const_value_t*>(var, get_constant(pcfg_n, var_reach_def)));
            }
        }
    }

    void store_dependency_relevant_vars(
            FTDGNode* ftdg_n,
            Node* ctx,
            bool true_edge,
            /*inout*/std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less>& fixed_relevant_vars,
            /*inout*/std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less>& variable_relevant_vars)
    {
        switch (ftdg_n->get_type())
        {
            case FTDGLoop:
            {
                const ObjectList<FTDGNode*>& inner = ftdg_n->get_inner();
                for (ObjectList<FTDGNode*>::const_iterator it = inner.begin();
                    it != inner.end(); ++it)
                {
                    store_dependency_relevant_vars(*it, ctx, true_edge, fixed_relevant_vars, variable_relevant_vars);
                }
                break;
            }
            case FTDGCondition:
            {
                const ObjectList<FTDGNode*>& inner = (true_edge ? ftdg_n->get_inner_true()
                                                                : ftdg_n->get_inner_false());
                for (ObjectList<FTDGNode*>::const_iterator it = inner.begin();
                    it != inner.end(); ++it)
                {
                    store_dependency_relevant_vars(*it, ctx, true_edge, fixed_relevant_vars, variable_relevant_vars);
                }
                break;
            }
            case FTDGTask:
            {
                Node* pcfg_n = ftdg_n->get_pcfg_node();
                NodeclMap& reach_defs_in = pcfg_n->get_reaching_definitions_in();
                // In all dependencies where the task is the source, store the variables in the LHS of the dependency expression
                const ObjectList<Edge*>& exits = pcfg_n->get_exit_edges();
                for (ObjectList<Edge*>::const_iterator it = exits.begin();
                        it != exits.end(); ++it)
                {
                    // This is not supported
                    ERROR_CONDITION((*it)->get_target()->is_omp_virtual_tasksync(),
                                    "Tasks not synchronized in the scope of their function is not supported for ETDG.\n", 0)

                    NBase cond = (*it)->get_condition();
                    if (cond.is_null())
                    {   // true edge
                        continue;
                    }
                    else
                    {
                        // Store LHS variables
                        LHSVisitor lhs_visit;
                        lhs_visit.walk(cond);
                        const std::vector<Nodecl::Symbol>& lhs_vars = lhs_visit.get_lhs_vars();
                        for (std::vector<Nodecl::Symbol>::const_iterator itv = lhs_vars.begin();
                            itv != lhs_vars.end(); ++itv)
                        {
                            store_dependency_relevant_vars_rec(*itv, ftdg_n, reach_defs_in, ctx,
                                                               fixed_relevant_vars, variable_relevant_vars);
                        }
                    }
                }
                // In all dependencies where the task is the target, store the variables in the RHS of the dependency expression
                const ObjectList<Edge*>& entries = pcfg_n->get_entry_edges();
                for (ObjectList<Edge*>::const_iterator it = entries.begin();
                        it != entries.end(); ++it)
                {
                    if ((*it)->get_source()->is_omp_task_creation_node())
                        continue;

                    NBase cond = (*it)->get_condition();
                    if (cond.is_null())
                    {   // true edge
                        continue;
                    }
                    else
                    {
                        // Store RHS variables
                        RHSVisitor rhs_visit;
                        rhs_visit.walk(cond);
                        const std::vector<Nodecl::Symbol>& rhs_vars = rhs_visit.get_rhs_vars();
                        for (std::vector<Nodecl::Symbol>::const_iterator itv = rhs_vars.begin();
                            itv != rhs_vars.end(); ++itv)
                        {
                            store_dependency_relevant_vars_rec(*itv, ftdg_n, reach_defs_in, ctx,
                                                               fixed_relevant_vars, variable_relevant_vars);
                        }
                    }
                }
                break;
            }
            case FTDGTaskwait:
            case FTDGBarrier:
            {
                // There is no variable to store here
                break;
            }
            default:
            {
                internal_error("Unexpected node type %s.\n", 0);
                break;
            }
        };
    }

    void propagate_constant_values(
        Nodecl::NodeclBase v,
        std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less>& fixed_relevant_vars)
    {
        for (std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less>::iterator itf = fixed_relevant_vars.begin();
             itf != fixed_relevant_vars.end(); ++itf)
        {
            if (Nodecl::Utils::nodecl_contains_nodecl_by_structure(/*haystack*/v, /*needle*/itf->first))
            {
                Nodecl::Utils::nodecl_replace_nodecl_by_structure(/*haystack*/v, /*needle*/itf->first,
                                                                  /*replacement*/Nodecl::NodeclBase(const_value_to_nodecl(itf->second)));
            }
        }
        Optimizations::ReduceExpressionVisitor rev;
        rev.walk(v);
    }

    void propagate_constant_values(
        Nodecl::NodeclBase v,
        std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less>& variable_relevant_vars)
    {
        for (std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less>::iterator itv = variable_relevant_vars.begin();
             itv != variable_relevant_vars.end(); ++itv)
        {
            if (Nodecl::Utils::nodecl_contains_nodecl_by_structure(/*haystack*/v, /*needle*/itv->first))
            {
                Nodecl::Utils::nodecl_replace_nodecl_by_structure(/*haystack*/v, /*needle*/itv->first,
                                                                  /*replacement*/itv->second);
            }
        }
        Optimizations::ReduceExpressionVisitor rev;
        rev.walk(v);
    }

    void propagate_constant_values(
            std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less>& fixed_relevant_vars,
            std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less>& variable_relevant_vars)
    {
        bool changes = true;
        Optimizations::ReduceExpressionVisitor rev;
        while (changes)
        {
            changes = false;
            for (std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less>::iterator itf = fixed_relevant_vars.begin();
                 itf != fixed_relevant_vars.end(); ++itf)
            {
                for (std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less>::iterator itv = variable_relevant_vars.begin();
                     itv != variable_relevant_vars.end(); ++itv)
                {
                    if (Nodecl::Utils::nodecl_contains_nodecl_by_structure(/*haystack*/itv->second, /*needle*/itf->first))
                    {
                        changes = true;
                        Nodecl::Utils::nodecl_replace_nodecl_by_structure(/*haystack*/itv->second, /*needle*/itf->first,
                                                                          /*replacement*/Nodecl::NodeclBase(const_value_to_nodecl(itf->second)));
                        rev.walk(itv->second);
                    }
                }
            }

            for (std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less>::iterator itv1 = variable_relevant_vars.begin();
                 itv1 != variable_relevant_vars.end(); ++itv1)
            {
                if (!itv1->second.is_constant())
                    continue;
                for (std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less>::iterator itv2 = variable_relevant_vars.begin();
                     itv2 != variable_relevant_vars.end(); ++itv2)
                {
                    if (Nodecl::Utils::structurally_equal_nodecls(itv1->first, itv2->first))
                        continue;

                    if (Nodecl::Utils::nodecl_contains_nodecl_by_structure(/*haystack*/itv2->second, /*needle*/itv1->first))
                    {
                        changes = true;
                        Nodecl::Utils::nodecl_replace_nodecl_by_structure(/*haystack*/itv2->second, /*needle*/itv1->first,
                                                                          /*replacement*/itv1->second);
                        rev.walk(itv2->second);
                    }
                }
            }
        }
    }

    void store_dependency_relevant_vars_and_fix(
            FTDGNode* ftdg_n,
            Node* ctx,
            bool true_edge,
            /*inout*/std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less>& fixed_relevant_vars,
            /*inout*/std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less>& variable_relevant_vars)
    {
        // Store the dependencies
        store_dependency_relevant_vars(ftdg_n, ctx, true_edge, fixed_relevant_vars, variable_relevant_vars);

        // Propagate constant values in the computed dependencies
        propagate_constant_values(fixed_relevant_vars, variable_relevant_vars);
    }

    void print_relevant_vars(
        std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> fixed_relevant_vars,
        std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less> variable_relevant_vars,
        std::string indent)
    {
        std::cerr << indent << "MAP : " << std::endl;
        std::cerr << indent << "   Fixed relevant vars : " << std::endl;
        for (std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less>::iterator it = fixed_relevant_vars.begin();
             it != fixed_relevant_vars.end(); ++it)
        {
            NBase const_var(const_value_to_nodecl(it->second));
            std::cerr << indent << "      -> " << it->first.prettyprint()
                      << " = " << const_var.prettyprint() << std::endl;
        }
        std::cerr << indent << "   Variable relevant vars : " << std::endl;
        for (std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less>::iterator it = variable_relevant_vars.begin();
             it != variable_relevant_vars.end(); ++it)
        {
            std::cerr << indent << "      -> " << it->first.prettyprint()
                      << " = " << it->second.prettyprint() << std::endl;
        }
    }

    NBase get_condition_stmts(Node* cond_node)
    {
        NBase cond_stmt;

        if(cond_node->is_graph_node())
            cond_stmt = cond_node->get_graph_related_ast();
        else
        {
            NodeclList stmts = cond_node->get_statements();
            ERROR_CONDITION(stmts.size()!=1,
                            "%s statements found in condition node %d. Only one statement expected.\n",
                            stmts.size(), cond_node->get_id());
            cond_stmt = stmts[0];
        }

        return cond_stmt;
    }

    unsigned tdg_id = 0;
    std::map<FTDGNode*, unsigned> _ftdg_task_to_tdg_id;
}

    SubETDG::SubETDG(unsigned maxI, unsigned maxT,
                     unsigned parent_tdg_id, const std::vector<FTDGNode*>& outermost_nodes)
        : _ftdg_outermost_nodes(outermost_nodes), _tdg_id(tdg_id++),
          _parent_tdg_id(parent_tdg_id), _maxI(maxI), _maxT(maxT),
          _roots(), _leafs(), _tasks(), _source_to_etdg_nodes()
    {}

    void SubETDG::expand_subtdg()
    {
        if (TDG_DEBUG)
            std::cerr << "****************** Expanding ETDG " << _tdg_id << " ******************" << std::endl;
        std::deque<unsigned> loops_ids;
        for (std::vector<FTDGNode*>::const_iterator it = _ftdg_outermost_nodes.begin();
             it != _ftdg_outermost_nodes.end(); ++it)
        {
            switch ((*it)->get_type())
            {
                case FTDGLoop:
                {
                    std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> current_relevant_vars;
                    expand_loop(*it, current_relevant_vars, loops_ids, "   ");
                    break;
                }
                case FTDGCondition:
                {
                    Node* cond_node = (*it)->get_pcfg_node()->get_condition_node();
                    NBase cond = get_condition_stmts(cond_node);
                    ObjectList<NBase> cond_syms = Nodecl::Utils::get_all_memory_accesses(cond);
                    std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> current_relevant_vars;
                    for (ObjectList<NBase>::iterator itc = cond_syms.begin(); itc != cond_syms.end(); ++itc)
                    {
                        current_relevant_vars[*itc] = get_constant(cond_node, *itc);
                    }
                    expand_condition(*it, current_relevant_vars, loops_ids, "   ");
                    break;
                }
                case FTDGTaskwait:
                case FTDGBarrier:
                {
                    sync_create_and_connect(*it, "   ");
                    break;
                }
                case FTDGTarget:
                {
                    internal_error("Unsupported node Target while expanding TDG.\n", 0);
                    break;
                }
                case FTDGTask:
                {
                    std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> fixed_relevant_vars;
                    std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less> variable_relevant_vars;
                    store_dependency_relevant_vars_and_fix(*it, (*it)->get_pcfg_node(), /*true_edge*/false,
                                                           fixed_relevant_vars, variable_relevant_vars);
                    task_create_and_connect(*it, fixed_relevant_vars, loops_ids, "   ");
                    break;
                }
                default:
                {
                    internal_error("Unexpected node %d while expanding TDG.\n", (*it)->get_type());
                }
            };
        }

        if (nt > 0)
            std::cerr << std::endl; // This print sets a line break after printing the ETDG progress

        if (TDG_DEBUG)
            std::cerr << "**************** END expanding ETDG " << _tdg_id << " ****************" << std::endl;
    }

    void SubETDG::expand_loop(
            FTDGNode* n,
            std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> current_relevant_vars,
            std::deque<unsigned>& loops_ids,
            std::string indent)
    {
        LoopInfo* current_loop_info = ftdgnode_to_loop_info.find(n)->second;
        Utils::InductionVar* iv = current_loop_info->_iv;
        Nodecl::NodeclBase lb = current_loop_info->_lb.shallow_copy();
        Nodecl::NodeclBase ub = current_loop_info->_ub.shallow_copy();
        Nodecl::NodeclBase incr = current_loop_info->_incr.shallow_copy();

        if (TDG_DEBUG)
        {
            std::cerr << indent << "Expanding loop " << n->get_pcfg_node()->get_graph_related_ast().get_locus_str() << std::endl;
            std::cerr << indent << "   IV = " << iv->get_variable().prettyprint() << std::endl;
            std::cerr << indent << "   LB = " << lb.prettyprint() << std::endl;
            std::cerr << indent << "   UB = " << ub.prettyprint() << std::endl;
            std::cerr << indent << "   INCR = " << incr.prettyprint() << std::endl;
            std::cerr << indent << "   NITER = " << current_loop_info->_niter << std::endl;
        }

        // Store the variables that will be involved in dependency expressions
        Node* pcfg_n = n->get_pcfg_node();
        std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> fixed_relevant_vars = current_relevant_vars;
        std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less> variable_relevant_vars;
        store_dependency_relevant_vars_and_fix(n, pcfg_n, /*true_edge*/false,
                                               fixed_relevant_vars, variable_relevant_vars);
        if (TDG_DEBUG)
            print_relevant_vars(fixed_relevant_vars, variable_relevant_vars, indent+"   ");

        // Replace, if necessary, the current values in the loop boundaries
        bool recompute_niter = false;
        if (!lb.is_constant())
        {
            propagate_constant_values(lb, fixed_relevant_vars);
            propagate_constant_values(lb, variable_relevant_vars);
            ERROR_CONDITION(!lb.is_constant(),
                            "Lower bound of loop %s could not be reduced to a constant",
                            n->get_pcfg_node()->get_graph_related_ast().get_locus_str().c_str());
            recompute_niter = true;
        }
        if (!ub.is_constant())
        {
            propagate_constant_values(ub, fixed_relevant_vars);
            propagate_constant_values(ub, variable_relevant_vars);
            ERROR_CONDITION(!ub.is_constant(),
                            "Upper bound of loop %s could not be reduced to a constant",
                            n->get_pcfg_node()->get_graph_related_ast().get_locus_str().c_str());
            recompute_niter = true;
        }
        if (!incr.is_constant())
        {
            propagate_constant_values(incr, fixed_relevant_vars);
            propagate_constant_values(incr, variable_relevant_vars);
            ERROR_CONDITION(!incr.is_constant(),
                            "Increment of loop %s could not be reduced to a constant",
                            n->get_pcfg_node()->get_graph_related_ast().get_locus_str().c_str());
            recompute_niter = true;
        }
        if (recompute_niter)
        {
            const_value_t* niterc = const_value_div(const_value_add(const_value_sub(ub.get_constant(),
                                                                                    lb.get_constant()),
                                                                    const_value_get_one(4, 1)),
                                                    incr.get_constant());
            unsigned niter = const_value_cast_to_unsigned_int(niterc);
            if (_maxI < niter)
                _maxI = niter;
        }

        const_value_t* c = lb.get_constant();
        NBase cn(const_value_to_nodecl(c));
        unsigned iter = 1;
        loops_ids.push_back(iter);
        const ObjectList<FTDGNode*>& inner = n->get_inner();
        while (const_value_is_zero(const_value_gt(c, ub.get_constant())))
        {
            if (TDG_DEBUG)
                std::cerr << indent << "   * IV " << iv->get_variable().prettyprint() << " = " << cn.prettyprint() << std::endl;

            for (ObjectList<FTDGNode*>::const_iterator it = inner.begin();
                 it != inner.end(); ++it)
            {
                std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> inner_relevant_vars = fixed_relevant_vars;
                // FIXME We are not considering variables that depend on the induction variable
                inner_relevant_vars.insert(std::pair<NBase, const_value_t*>(iv->get_variable(), c));

                FTDGNodeType it_type = (*it)->get_type();
                switch (it_type)
                {
                    case FTDGLoop:
                    {
                        expand_loop(*it, inner_relevant_vars, loops_ids, indent+"      ");
                        break;
                    }
                    case FTDGCondition:
                    {
                        expand_condition(*it, inner_relevant_vars, loops_ids, indent+"      ");
                        break;
                    }
                    case FTDGTarget:
                    {
                        internal_error("Target not yet supported for etdg.\n", 0);
                        break;
                    }
                    case FTDGTaskwait:
                    case FTDGBarrier:
                    {
                        sync_create_and_connect(*it, indent+"      ");
                        break;
                    }
                    case FTDGTask:
                    {
                        task_create_and_connect(*it, inner_relevant_vars, loops_ids, indent+"      ");
                        break;
                    }
                    default:
                    {
                        internal_error("Unexpected FTDGNode type %d.\n", it_type);
                    }
                };
            }

            ++iter;
            loops_ids.pop_back();
            loops_ids.push_back(iter);
            c = const_value_add(c, incr.get_constant());
            cn = NBase(const_value_to_nodecl(c));
        }
        loops_ids.pop_back();
    }

    void SubETDG::expand_condition(
            FTDGNode* n,
            std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> current_relevant_vars,
            std::deque<unsigned>& loops_ids,
            std::string indent)
    {
        if (TDG_DEBUG)
            std::cerr << indent << "Expanding condition " << n->get_pcfg_node()->get_graph_related_ast().get_locus_str() << std::endl;

        Node* pcfg_n = n->get_pcfg_node();

        // Evaluate the condition
        Node* cond_node = pcfg_n->get_condition_node();
        NBase cond = get_condition_stmts(cond_node);
        ReplaceAndEvalVisitor rev(/*lhs*/ current_relevant_vars, /*rhs*/ current_relevant_vars);
        bool res = rev.walk(cond.shallow_copy());
        if (TDG_DEBUG)
            std::cerr << indent << "   IfElse node " << pcfg_n->get_id() << " with codition '"
                      << cond.prettyprint() << "' evaluates to " << res << std::endl;

        // If the condition evaluates to true, then create tasks within the true branch
        // otherwise create tasks within the false edge
        ObjectList<FTDGNode*> inner;
        std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> fixed_relevant_vars = current_relevant_vars;
        std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less> variable_relevant_vars;
        if (res)
        {
            inner = n->get_inner_true();
            store_dependency_relevant_vars_and_fix(n, pcfg_n, /*true_edge*/true,
                                                   fixed_relevant_vars, variable_relevant_vars);
        }
        else
        {
            inner = n->get_inner_false();
            store_dependency_relevant_vars_and_fix(n, pcfg_n, /*true_edge*/false,
                                                   fixed_relevant_vars, variable_relevant_vars);
        }
        if (TDG_DEBUG)
            print_relevant_vars(fixed_relevant_vars, variable_relevant_vars, indent+"   ");

        ERROR_CONDITION(!variable_relevant_vars.empty(),
                        "Variable relevant variables found when expanding condition. This is not yet supported.\n", 0);

        for (ObjectList<FTDGNode*>::const_iterator it = inner.begin();
                it != inner.end(); ++it)
        {
            FTDGNodeType it_type = (*it)->get_type();
            switch (it_type)
            {
                case FTDGLoop:
                {
                    internal_error("Nested loop not yet supported for etdg\n", 0);
                    expand_loop(*it, fixed_relevant_vars, loops_ids, indent+"      ");
                    break;
                }
                case FTDGCondition:
                {
                    expand_condition(*it, fixed_relevant_vars, loops_ids, indent+"      ");
                    break;
                }
                case FTDGTarget:
                {
                    internal_error("Target not yet supported for etdg.\n", 0);
                    break;
                }
                case FTDGTaskwait:
                case FTDGBarrier:
                {
                    sync_create_and_connect(*it, indent+"      ");
                    break;
                }
                case FTDGTask:
                {
                    task_create_and_connect(*it, fixed_relevant_vars, loops_ids, indent+"      ");
                    break;
                }
                default:
                {
                    internal_error("Unexpected FTDGNode type %d.\n", it_type);
                }
            };
        }

        if (TDG_DEBUG)
            std::cerr << "   **** END expanding condition ****" << std::endl;
    }

    void SubETDG::connect_nodes(ETDGNode* source, ETDGNode* target, std::string indent)
    {
        source->add_output(target);
        target->add_input(source);
        if (TDG_DEBUG)
            std::cerr << indent << "Connecting " << source->get_id() << " -> " << target->get_id() << std::endl;
    }

    void SubETDG::disconnect_nodes(ETDGNode* source, ETDGNode* target, std::string indent)
    {
        source->remove_output(target);
        target->remove_input(source);
        if (TDG_DEBUG)
            std::cerr << indent << "Disconnecting " << source->get_id() << " -> " << target->get_id() << std::endl;
    }

    ETDGNode* SubETDG::create_task_node(FTDGNode* ftdg_n, std::deque<unsigned> loops_ids, std::string indent)
    {
        ERROR_CONDITION(ftdg_n->get_type() != FTDGTask,
                        "Unsuported type %d for an ETDGNode. Only tasks accepted\n",
                        ftdg_n->get_type());

        Node* source_node = ftdg_n->get_pcfg_node();
        Nodecl::NodeclBase source_task = source_node->get_graph_related_ast();
        ERROR_CONDITION(!source_task.is<Nodecl::OpenMP::Task>(),
                        "The extensible graph node %d related with an ETDG task node has wrong type %s",
                        ftdg_n->get_pcfg_node()->get_id(),
                        ast_print_node_type(source_task.get_kind()));
        unsigned etask_id = get_etdg_node_id(ftdgnode_to_task_id.find(ftdg_n)->second, loops_ids);
        ETDGNode* etdg_n = new ETDGNode(etask_id, source_node);
        _source_to_etdg_nodes[source_task].append(etdg_n);

        if (TDG_DEBUG)
            std::cerr << indent << "Created task node " << etdg_n->get_id() << " with related pcfg node " << ftdg_n->get_pcfg_node()->get_id() << std::endl;

        _tasks.insert(etdg_n);
        _leafs.insert(etdg_n);

        return etdg_n;
    }

    unsigned SubETDG::get_etdg_node_id(unsigned task_id, std::deque<unsigned> loops_ids)
    {
//         if (TDG_DEBUG)
//             std::cerr << "TASK " << task_id << "(";
        unsigned sum = 0;
        while (!loops_ids.empty())
        {
//             if (TDG_DEBUG)
//                 std::cerr << loops_ids.back() << ", ";
            sum = (sum + loops_ids.back()) * _maxI;
            loops_ids.pop_back();
        }
//         if (TDG_DEBUG)
//             std::cerr << ")  ->  " << task_id + (_maxT * sum) << std::endl;
        return task_id + (_maxT * sum);
    }

namespace {
    void remove_ancestors_from_set(ETDGNode* n, std::set<ETDGNode*>& s)
    {
        std::set<ETDGNode*> n_ins = n->get_inputs();
        for (std::set<ETDGNode*>::iterator it = n_ins.begin(); it != n_ins.end(); ++it)
        {
            if (s.find(*it) != s.end())
            {
                s.erase(*it);
                remove_ancestors_from_set(*it, s);
            }
        }
    }
}

    bool SubETDG::compute_task_connections(
            ETDGNode* possible_source,
            ETDGNode* target,
            std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> target_vars_map,
            std::set<ETDGNode*>& all_possible_ancestors,
            std::string indent)
    {
        // @possible_source is not a candidate to be an ancestor of @target
        if (all_possible_ancestors.find(possible_source) == all_possible_ancestors.end())
            return false;

        // Check the dependency replacing all variables with the corresponding constant values in the dependency expression
        bool res;
        Node* possible_source_pcfg_node = possible_source->get_pcfg_node();
        Node* target_pcfg_node = target->get_pcfg_node();
        ObjectList<Node*> children = possible_source_pcfg_node->get_children();
        Edge* edge = NULL;
        NBase cond;
        if (!children.find(target_pcfg_node).empty())
        {
            edge = ExtensibleGraph::get_edge_between_nodes(possible_source_pcfg_node, target_pcfg_node);
            cond = edge->get_condition();
        }
        if (edge == NULL /*this a fabricated edge between nodes from different nesting regions*/
            || cond.is_null() /*the edge is unconditional*/)
        {
            res = true;
        }
        else
        {
            std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> possible_source_vars_map = possible_source->get_vars_map();
            ReplaceAndEvalVisitor rev(/*lhs*/ possible_source_vars_map, /*rhs*/ target_vars_map);
            NBase cond_cp = cond.shallow_copy();
            res = rev.walk(cond_cp);
            nodecl_free(cond_cp.get_internal_nodecl());
        }

        // If the condition evaluates to true, then
        //   - connect the nodes
        //   - remove all @possible_source ancestors from the list of possible ancestors
        // otherwise, keep traversing bottom-top the ETDG
        if (res)
        {
            connect_nodes(possible_source, target, indent);
            remove_ancestors_from_set(possible_source, all_possible_ancestors);
            _leafs.erase(possible_source);
        }

        return res;
    }

    void SubETDG::connect_task_node(ETDGNode* etdg_n, FTDGNode* ftdg_n, std::string indent)
    {
        // Gather all possible ancestors
        std::set<ETDGNode*> current_tdg_possible_ancestors;
        std::set<ETDGNode*> other_tdg_possible_ancestors;
        const ObjectList<FTDGNode*> predecessors = ftdg_n->get_predecessors();
        for (ObjectList<FTDGNode*>::const_iterator it = predecessors.begin(); it != predecessors.end(); ++it)
        {
            std::map<FTDGNode*, std::set<ETDGNode*> >::iterator itm = ftdg_to_etdg_nodes.find(*it);
            // Its parents have not been created yet
            if (itm == ftdg_to_etdg_nodes.end())
                continue;

            std::set<ETDGNode*> it_possible_ancestors = itm->second;
            if (ftdg_n->get_parent() == (*it)->get_parent())
            {
                current_tdg_possible_ancestors.insert(it_possible_ancestors.begin(),
                                                    it_possible_ancestors.end());
            }
            else
            {
                other_tdg_possible_ancestors.insert(it_possible_ancestors.begin(),
                                                    it_possible_ancestors.end());
            }
        }

        // Connect the node with previous dependences/synchronizations
        std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> etdg_n_vars_map = etdg_n->get_vars_map();
            // Connect it with other nodes from the same TDG depending on the predicates
        std::deque<ETDGNode*> nlist(_leafs.begin(), _leafs.end());
        std::set<ETDGNode*> already_visited;
        while (!nlist.empty())
        {
            ETDGNode* possible_source = nlist.front();
            nlist.pop_front();
            if (already_visited.find(possible_source) != already_visited.end())
                continue;
            already_visited.insert(possible_source);

            // Do not connect a ETDG node with itself
            if (possible_source == etdg_n)
                continue;

            bool connected = compute_task_connections(possible_source, etdg_n, etdg_n_vars_map, current_tdg_possible_ancestors, indent);
            if (!connected)
            {
                std::set<ETDGNode*> inputs = possible_source->get_inputs();
                for (std::set<ETDGNode*>::iterator it = inputs.begin(); it != inputs.end(); ++it)
                {
                    nlist.push_back(*it);
                }
            }
        }
            // Connect it with nodes from other TDGs depending on the predicates
        for (std::set<ETDGNode*>::iterator it = other_tdg_possible_ancestors.begin();
             it != other_tdg_possible_ancestors.end(); ++it)
        {
            compute_task_connections(*it, etdg_n, etdg_n_vars_map, other_tdg_possible_ancestors, indent);
        }

        // Connect the node with previous synchronizations from the same TDG
        if (etdg_n->get_inputs().empty())
        {
            // The only entry node is the creation of the task
            _roots.insert(etdg_n);
        }
    }

    void SubETDG::remove_task_transitive_inputs(ETDGNode* n)
    {
        const std::set<ETDGNode*>& inputs = n->get_inputs();
        for (std::set<ETDGNode*>::const_iterator its = inputs.begin(); its != inputs.end(); ++its)
        {
            for (std::set<ETDGNode*>::const_iterator itt = inputs.begin(); itt != inputs.end(); ++itt)
            {
                if (*its == *itt)
                    continue;

                if (is_ancestor(*its, *itt))
                {
                    disconnect_nodes(*its, n, "   ");
                }
            }
        }
    }

    void SubETDG::task_create_and_connect(
            FTDGNode* ftdg_n,
            std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> current_relevant_vars,
            std::deque<unsigned> loops_ids,
            std::string indent)
    {
        if (nt == 0)
            std::cerr << indent << "Tasks expansion progress (this may take some time)" << std::endl;
        std::cerr << '\r' << std::setw(6) << ++nt << std::flush;
        ETDGNode* etdg_n = create_task_node(ftdg_n, loops_ids, indent);
        etdg_n->set_vars_map(current_relevant_vars);

        ftdg_to_etdg_nodes[ftdg_n].insert(etdg_n);
        connect_task_node(etdg_n, ftdg_n, indent);

        remove_task_transitive_inputs(etdg_n);

        _ftdg_task_to_tdg_id[ftdg_n] = _tdg_id;
    }

    static int sync_id = -1;
    ETDGNode* SubETDG::create_sync_node(
            FTDGNode* ftdg_n,
            std::string indent)
    {
        ERROR_CONDITION(ftdg_n->get_type() != FTDGTaskwait && ftdg_n->get_type() != FTDGBarrier,
                        "Unsuported type %d for an ETDGNode. Only tasks accepted\n",
                        ftdg_n->get_type());
        if (TDG_DEBUG)
            std::cerr << indent << "Created sync node " << sync_id << " with related pcfg node " << ftdg_n->get_pcfg_node()->get_id() << std::endl;
        return new ETDGNode(sync_id--, ftdg_n->get_pcfg_node());
    }

    void SubETDG::connect_sync_node(ETDGNode* etdg_n, std::string indent)
    {
        for (std::set<ETDGNode*>::iterator it = _leafs.begin(); it != _leafs.end(); ++it)
        {
            connect_nodes(*it, etdg_n, indent);
        }
        _leafs.clear();
        _leafs.insert(etdg_n);
    }

    void SubETDG::sync_create_and_connect(
            FTDGNode* ftdg_n,
            std::string indent)
    {
        ETDGNode* etdg_n = create_sync_node(ftdg_n, indent);

        ftdg_to_etdg_nodes[ftdg_n].insert(etdg_n);

        connect_sync_node(etdg_n, indent);
    }

    bool SubETDG::is_ancestor(ETDGNode* source, ETDGNode* target)
    {
        const std::set<ETDGNode*>& outputs = source->get_outputs();
        if (outputs.find(target) != outputs.end())
            return true;

        for (std::set<ETDGNode*>::const_iterator it = outputs.begin(); it != outputs.end(); ++it)
        {
            if (is_ancestor(*it, target))
                return true;
        }

        return false;
    }

    void SubETDG::remove_synchronizations_rec(ETDGNode* n)
    {
        if (n->is_visited())
            return;
        n->set_visited(true);

        // Note: get here the list of outputs, before the node may be removed and disconnected
        const std::set<ETDGNode*>& outputs = n->get_outputs();
        if (n->get_id() < 0)
        {
            // Disconnect the node from its parents and children and remove it
            const std::set<ETDGNode*>& inputs = n->get_inputs();
            for (std::set<ETDGNode*>::const_iterator it = inputs.begin(); it != inputs.end(); ++it)
            {
                disconnect_nodes(*it, n, "   ");
            }
            for (std::set<ETDGNode*>::const_iterator it = outputs.begin(); it != outputs.end(); ++it)
            {
                disconnect_nodes(n, *it, "   ");
            }

            // Note that a node may be a leaf and have children at the same time
            // This happens only if the children are from a different TDG

            if (_leafs.find(n) != _leafs.end())
            {   // Restore the list of leafs
                _leafs.erase(n);
                for (std::set<ETDGNode*>::const_iterator it = inputs.begin(); it != inputs.end(); ++it)
                {
                    _leafs.insert(*it);
                }
            }

            // Connect all parents with all children
            for (std::set<ETDGNode*>::const_iterator its = inputs.begin(); its != inputs.end(); ++its)
            {
                for (std::set<ETDGNode*>::const_iterator itt = outputs.begin(); itt != outputs.end(); ++itt)
                {
                    connect_nodes(*its, *itt, "   ");
                }
            }

            // Remove the node
            delete n;
        }

        for (std::set<ETDGNode*>::const_iterator it = outputs.begin(); it != outputs.end(); ++it)
            remove_synchronizations_rec(*it);
    }

    void SubETDG::remove_synchronizations()
    {
        for (ObjectList<ETDGNode*>::iterator it = _roots.begin(); it != _roots.end(); ++it)
            remove_synchronizations_rec(*it);
    }

    void SubETDG::purge_subtdg()
    {
        if (TDG_DEBUG)
            std::cerr << "****************** Removing synchronizations from ETDG " << _tdg_id << " ******************" << std::endl;
        remove_synchronizations();
        if (TDG_DEBUG)
            std::cerr << "**************** END removing synchronizations from ETDG " << _tdg_id << " ****************" << std::endl;
        clear_visits();
    }

    void SubETDG::clear_visits_rec(ETDGNode* n)
    {
        if (!n->is_visited())
            return;

        n->set_visited(false);

        const std::set<ETDGNode*>& outputs = n->get_outputs();
        for (std::set<ETDGNode*>::const_iterator it = outputs.begin(); it != outputs.end(); ++it)
        {
            clear_visits_rec(*it);
        }
    }

    void SubETDG::clear_visits()
    {
        for (ObjectList<ETDGNode*>::iterator it = _roots.begin(); it != _roots.end(); ++it)
        {
            clear_visits_rec(*it);
        }
    }
    unsigned SubETDG::get_tdg_id() const
    {
        return _tdg_id;
    }

    unsigned SubETDG::get_parent_tdg_id() const
    {
        return _parent_tdg_id;
    }

    void SubETDG::set_parent_tdg_id(unsigned parent_tdg_id)
    {
        _parent_tdg_id = parent_tdg_id;
    }

    unsigned SubETDG::get_maxI() const
    {
        return _maxI;
    }

    unsigned SubETDG::get_maxT() const
    {
        return _maxT;
    }

    const ObjectList<ETDGNode*>& SubETDG::get_roots() const
    {
        return _roots;
    }

    const std::set<ETDGNode*>& SubETDG::get_leafs() const
    {
        return _leafs;
    }

    const ObjectList<ETDGNode*>& SubETDG::get_tasks() const
    {
        return _tasks;
    }

    unsigned SubETDG::get_nTasks() const
    {
        return _tasks.size();
    }

    const std::map<Nodecl::NodeclBase, ObjectList<ETDGNode*> >&
             SubETDG::get_source_to_etdg_nodes() const
    {
        return _source_to_etdg_nodes;
    }

    ExpandedTaskDependencyGraph::ExpandedTaskDependencyGraph(ExtensibleGraph* pcfg)
        : _ftdg(NULL), _etdgs(), _maxI(0), _maxT(0)
    {
        _ftdg = new FlowTaskDependencyGraph(pcfg);
        if (TDG_DEBUG)
            _ftdg->print_tdg_to_dot();
        compute_constants();
        expand_tdg();
    }

    void ExpandedTaskDependencyGraph::compute_constants_rec(FTDGNode* n)
    {
        switch (n->get_type())
        {
            case FTDGLoop:
            {
                Node* pcfg_n = n->get_pcfg_node();
                Utils::InductionVarList& ivs = pcfg_n->get_induction_variables();
                ERROR_CONDITION(ivs.size() != 1,
                                "ETDG does not support loops with more than one Induction Variable.\n",
                                0);

                Utils::InductionVar* iv = ivs[0];
                NodeclSet lbs = iv->get_lb();
                NodeclSet ubs = iv->get_ub();
                ERROR_CONDITION(lbs.size()!=1 || ubs.size()!=1,
                                "Induction variable %s has an unsupported behavior\n",
                                iv->get_variable().prettyprint().c_str());

                Nodecl::NodeclBase lb = reduce_to_constant_as_possible(pcfg_n, *lbs.begin());
                Nodecl::NodeclBase ub = reduce_to_constant_as_possible(pcfg_n, *ubs.begin());
                Nodecl::NodeclBase incr = reduce_to_constant_as_possible(pcfg_n, iv->get_increment());
                unsigned niter = 0;
                if (lb.is_constant() && ub.is_constant() && incr.is_constant())
                {
                    const_value_t* niterc = const_value_div(const_value_add(const_value_sub(ub.get_constant(),
                                                                                            lb.get_constant()),
                                                                            const_value_get_one(4, 1)),
                                                            incr.get_constant());
                    niter = const_value_cast_to_unsigned_int(niterc);
                }
                if (_maxI < niter)
                    _maxI = niter;

                LoopInfo* li = new LoopInfo(
                        iv, lb, ub, incr, 0);
                ftdgnode_to_loop_info[n] = li;

                // NOTE: No break here because we still have to traverse inner nodes
            }
            case FTDGCondition:
            {
                const ObjectList<FTDGNode*>& inner = n->get_inner();
                for (ObjectList<FTDGNode*>::const_iterator it = inner.begin(); it != inner.end(); ++it)
                    compute_constants_rec(*it);
                break;
            }
            case FTDGTask:
            {
                ftdgnode_to_task_id[n] = ++task_id;

                if (_maxT < task_id)
                    _maxT = task_id;
                break;
            }
            default:
            {}  // Nothing to do
        };
    }
    void ExpandedTaskDependencyGraph::compute_constants()
    {
        const std::vector<std::vector<FTDGNode*> >& ftdg_outermost_nodes = _ftdg->get_outermost_nodes();
        for (std::vector<std::vector<FTDGNode*> >::const_iterator it = ftdg_outermost_nodes.begin();
             it != ftdg_outermost_nodes.end(); ++it)
        {
            std::vector<FTDGNode*> current_outermost_nodes = *it;
            for (std::vector<FTDGNode*>::const_iterator itt = current_outermost_nodes.begin();
                itt != current_outermost_nodes.end(); ++itt)
            {
                compute_constants_rec(*itt);
            }
        }

        if (TDG_DEBUG)
            std::cerr << "_maxT = " << _maxT << ". _maxI = " << _maxI << std::endl;
    }

    void ExpandedTaskDependencyGraph::expand_tdg()
    {
        const std::vector<FTDGNode*>& ftdg_parents = _ftdg->get_parents();
        const std::vector<std::vector<FTDGNode*> >& ftdg_outermost_nodes = _ftdg->get_outermost_nodes();
        ERROR_CONDITION(ftdg_parents.size() != ftdg_outermost_nodes.size(),
                        "FTDG corrupted: The number of parents (%d) is different from the number of sets of outermost nodes (%d).\n",
                        ftdg_parents.size(), ftdg_outermost_nodes.size());

        // Perform reverse iteration, so nested regions are computed before outer regions
        // Otherwise some predecessors may not have been created
        std::vector<std::vector<FTDGNode*> >::const_reverse_iterator ito = ftdg_outermost_nodes.rbegin();
        for (; ito != ftdg_outermost_nodes.rend(); ++ito)
        {
            SubETDG* it_etdg = new SubETDG(_maxI, _maxT, /*default parent_tdg_id*/ 0, *ito);
            it_etdg->expand_subtdg();
            _etdgs.push_back(it_etdg);
        }

        // Set parenting relationships for nested regions
        // The set of etdgs is in reverse order regarding the set of parents
        // due to the reverse traversal made on the set of outermost nodes
        std::vector<FTDGNode*>::const_iterator itp = ftdg_parents.begin();
        for (std::vector<SubETDG*>::reverse_iterator it_etdg = _etdgs.rbegin();
             it_etdg != _etdgs.rend(); ++it_etdg, ++itp)
        {
            if (_ftdg_task_to_tdg_id.find(*itp) != _ftdg_task_to_tdg_id.end())
            {
                unsigned parent_tdg_id = _ftdg_task_to_tdg_id[*itp];
                (*it_etdg)->set_parent_tdg_id(parent_tdg_id);
                std::set<ETDGNode*> etdg_nodes = ftdg_to_etdg_nodes[*itp];
                ERROR_CONDITION(etdg_nodes.size() != 1,
                                "FTDG node %d has nested tasks and expands to more than one ETDG. This is not yet supported.\n",
                                (*itp)->get_id());
                (*etdg_nodes.begin())->set_child(*it_etdg);
            }
        }
        // Purge all TDGs from their synchronization nodes
        for (std::vector<SubETDG*>::reverse_iterator it_etdg = _etdgs.rbegin();
             it_etdg != _etdgs.rend(); ++it_etdg)
        {
            (*it_etdg)->purge_subtdg();

            if (TDG_DEBUG)
            {
                std::cerr << "****************** Summary of ETDG " << (*it_etdg)->get_tdg_id() << " ******************" << std::endl;
                std::cerr << "      List of roots:" << std::endl;
                const ObjectList<ETDGNode*>& roots = (*it_etdg)->get_roots();
                for (ObjectList<ETDGNode*>::const_iterator it = roots.begin(); it != roots.end(); ++it)
                {
                    std::cerr << "         - " << (*it)->get_id() << std::endl;
                }
                std::cerr << "      List of leafs:" << std::endl;
                const std::set<ETDGNode*>& leafs = (*it_etdg)->get_leafs();
                for (std::set<ETDGNode*>::const_iterator it = leafs.begin(); it != leafs.end(); ++it)
                {
                    std::cerr << "         - " << (*it)->get_id() << std::endl;
                }
                std::cerr << "**************** END summary of ETDG " << (*it_etdg)->get_tdg_id() << " ****************" << std::endl;
            }
        }
    }

    FlowTaskDependencyGraph* ExpandedTaskDependencyGraph::get_ftdg() const
    {
        return _ftdg;
    }

    const std::vector<SubETDG*>& ExpandedTaskDependencyGraph::get_etdgs() const
    {
        return _etdgs;
    }

    unsigned ExpandedTaskDependencyGraph::get_maxI() const
    {
        return _maxI;
    }

    unsigned ExpandedTaskDependencyGraph::get_maxT() const
    {
        return _maxT;
    }

}
}
