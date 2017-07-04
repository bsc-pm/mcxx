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


#include "cxx-cexpr.h"
#include "tl-counters.hpp"
#include "tl-analysis-utils.hpp"
#include "tl-expression-reduction.hpp"
#include "tl-task-dependency-graph.hpp"
#include "tl-induction-variables-data.hpp"

namespace TL {
namespace Analysis {

    static unsigned nt = 0;

    struct LoopInfo {
        Utils::InductionVar* _iv;
        NBase _lb;
        NBase _ub;
        NBase _incr;
        unsigned _niter;

        LoopInfo(Utils::InductionVar* iv, NBase lb, NBase ub, NBase incr, unsigned niter)
            : _iv(iv), _lb(lb), _ub(ub), _incr(incr), _niter(niter)
        {}
    };

namespace {
    std::map<Node*, std::vector<ETDGNode*> > pcfg_to_etdg_nodes;
    ETDGNode* last_synchronization = NULL;
    unsigned task_id = 0;

    std::map<FTDGNode*, LoopInfo*> ftdgnode_to_loop_info;
    std::map<FTDGNode*, unsigned> ftdgnode_to_task_id;

    const_value_t* get_constant(Node* n, NBase st)
    {
        if (st.is_constant())
            return st.get_constant();

        NBase malleable_st = st.shallow_copy();
        NodeclList mem_accesses = Nodecl::Utils::get_all_memory_accesses(st);
        for (NodeclList::iterator it = mem_accesses.begin(); it != mem_accesses.end(); ++it)
        {
            NBase var = *it;
            NodeclMap& rd_in = n->get_reaching_definitions_in();
            NodeclMap::iterator var_rd_in_it = rd_in.find(var);
            ERROR_CONDITION(rd_in.count(var) > 1,
                            "More than one reaching definition found for symbol %s.\n",
                            var.prettyprint().c_str());
            if (var_rd_in_it != rd_in.end())
            {
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
            else
            {
                internal_error("No reaching definition for variable %s in node %d.\n",
                               var.prettyprint().c_str(), n->get_id());
            }
        }

        Optimizations::ReduceExpressionVisitor rev;
        rev.walk(malleable_st);
        ERROR_CONDITION(!malleable_st.is_constant(),
                        "Expression %s could not be reduced to a constant.\n",
                        st.prettyprint().c_str());

        return malleable_st.get_constant();
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

        NBase var_reach_def = var_reach_def_it->second.first;
        // If we are in a loop, some variables will depend on the iteration
        // Otherwise, all variables must be fixed at this moment
        if (ctx->is_loop_node())
        {
            Scope ctx_sc = ctx->get_graph_related_ast().retrieve_context();
            if (var.get_symbol().get_scope().scope_is_enclosed_by(ctx_sc))
            {
                internal_error("Task is using a variable that is created within the expanded context. This is not supported yet.\n", 0);
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

    void print_relevant_vars(
        std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> fixed_relevant_vars,
        std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less> variable_relevant_vars)
    {
        std::cerr << "       MAP : " << std::endl;
        std::cerr << "           Fixed relevant vars : " << std::endl;
        for (std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less>::iterator it = fixed_relevant_vars.begin();
             it != fixed_relevant_vars.end(); ++it)
        {
            NBase const_var(const_value_to_nodecl(it->second));
            std::cerr << "                 -> " << it->first.prettyprint()
                      << " = " << const_var.prettyprint() << std::endl;
        }
        std::cerr << "           Variable relevant vars : " << std::endl;
        for (std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less>::iterator it = variable_relevant_vars.begin();
             it != variable_relevant_vars.end(); ++it)
        {
            std::cerr << "                 -> " << it->first.prettyprint()
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
}

    ExpandedTaskDependencyGraph::ExpandedTaskDependencyGraph(ExtensibleGraph* pcfg)
        : _ftdg(NULL), _maxI(0), _maxT(0), _roots(), _leafs(), _tasks(), _source_to_etdg_nodes()
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
                                "Psocrates does not support loops with more than one Induction Variable.\n",
                                0);

                Utils::InductionVar* iv = ivs[0];
                NodeclSet lbs = iv->get_lb();
                NodeclSet ubs = iv->get_ub();
                ERROR_CONDITION(lbs.size()!=1 || ubs.size()!=1,
                                "Induction variable %s has an unsupported behavior\n",
                                iv->get_variable().prettyprint().c_str());

                const_value_t* lbc = get_constant(pcfg_n, *lbs.begin());
                const_value_t* ubc = get_constant(pcfg_n, *ubs.begin());
                const_value_t* incrc = get_constant(pcfg_n, iv->get_increment());
                const_value_t* niterc = const_value_div(const_value_add(const_value_sub(ubc, lbc),
                                                                        const_value_get_one(4, 1)),
                                                        incrc);
                unsigned niter = const_value_cast_to_unsigned_int(niterc);
                if (_maxI < niter)
                    _maxI = niter;

                LoopInfo* li = new LoopInfo(
                        iv,
                        NBase(const_value_to_nodecl(lbc)),
                        NBase(const_value_to_nodecl(ubc)),
                        NBase(const_value_to_nodecl(incrc)),
                        niter);
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

                if (_maxT < task_id + 2)
                    _maxT = task_id + 2 /*Historially, we have added 2, but don't really know why :-S*/;
                break;
            }
            default:
            {}  // Nothing to do
        };
    }
    void ExpandedTaskDependencyGraph::compute_constants()
    {
        const std::vector<FTDGNode*>& ftdg_outermost_nodes = _ftdg->get_outermost_nodes();
        for (std::vector<FTDGNode*>::const_iterator it = ftdg_outermost_nodes.begin();
             it != ftdg_outermost_nodes.end(); ++it)
        {
            compute_constants_rec(*it);
        }

        if (TDG_DEBUG)
            std::cerr << "_maxT = " << _maxT << ". _maxI = " << _maxI << std::endl;
    }

    void ExpandedTaskDependencyGraph::expand_tdg()
    {
        std::deque<unsigned> loops_ids;
        const std::vector<FTDGNode*>& ftdg_outermost_nodes = _ftdg->get_outermost_nodes();
        for (std::vector<FTDGNode*>::const_iterator it = ftdg_outermost_nodes.begin();
             it != ftdg_outermost_nodes.end(); ++it)
        {
            switch ((*it)->get_type())
            {
                case FTDGLoop:
                {
                    std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> current_relevant_vars;
                    expand_loop(*it, current_relevant_vars, loops_ids);
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
                    expand_condition(*it, current_relevant_vars, loops_ids);
                    break;
                }
                case FTDGTaskwait:
                case FTDGBarrier:
                {
                    sync_create_and_connect(*it);
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
                    store_dependency_relevant_vars(*it, (*it)->get_pcfg_node(), /*true_edge*/false,
                                                   fixed_relevant_vars, variable_relevant_vars);
                    task_create_and_connect(*it, fixed_relevant_vars, loops_ids);
                    break;
                }
                default:
                {
                    internal_error("Unexpected node %d while expanding TDG.\n", (*it)->get_type());
                }
            };
        }

        purge_etdg();

        if (TDG_DEBUG)
        {
            std::cerr << "List of roots:" << std::endl;
            for (ObjectList<ETDGNode*>::iterator it = _roots.begin(); it != _roots.end(); ++it)
            {
                std::cerr << "   - " << (*it)->get_id() << std::endl;
            }
            std::cerr << "List of leafs:" << std::endl;
            for (std::set<ETDGNode*>::iterator it = _leafs.begin(); it != _leafs.end(); ++it)
            {
                std::cerr << "   - " << (*it)->get_id() << std::endl;
            }
        }

        if (nt > 0)
            std::cerr << std::endl; // This print sets a line break after printing the ETDG progress
    }

    void ExpandedTaskDependencyGraph::expand_loop(
            FTDGNode* n,
            std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> current_relevant_vars,
            std::deque<unsigned>& loops_ids)
    {
        if (TDG_DEBUG)
            std::cerr << "****************** Expanding loop ******************" << std::endl;

        LoopInfo* current_loop_info = ftdgnode_to_loop_info.find(n)->second;
        Utils::InductionVar* iv = current_loop_info->_iv;

        if (TDG_DEBUG)
        {
            std::cerr << "       LB = " << current_loop_info->_lb.prettyprint() << std::endl;
            std::cerr << "       UB = " << current_loop_info->_ub.prettyprint() << std::endl;
            std::cerr << "       INCR = " << current_loop_info->_incr.prettyprint() << std::endl;
            std::cerr << "       NITER = " << current_loop_info->_niter << std::endl;
        }

        // Store the variables that will be involved in dependency expressions
        Node* pcfg_n = n->get_pcfg_node();
        std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> fixed_relevant_vars = current_relevant_vars;
        std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less> variable_relevant_vars;
        store_dependency_relevant_vars(n, pcfg_n, /*true_edge*/false,
                                       fixed_relevant_vars, variable_relevant_vars);
        if (TDG_DEBUG)
            print_relevant_vars(fixed_relevant_vars, variable_relevant_vars);

        const_value_t* c = current_loop_info->_lb.get_constant();
        NBase cn(const_value_to_nodecl(c));
        unsigned iter = 1;
        loops_ids.push_back(iter);
        const ObjectList<FTDGNode*>& inner = n->get_inner();
        while (const_value_is_zero(const_value_gt(c, current_loop_info->_ub.get_constant())))
        {
            if (TDG_DEBUG)
                std::cerr << "* IV " << iv->get_variable().prettyprint() << " = " << cn.prettyprint() << std::endl;

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
                        expand_loop(*it, inner_relevant_vars, loops_ids);
                        break;
                    }
                    case FTDGCondition:
                    {
                        expand_condition(*it, inner_relevant_vars, loops_ids);
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
                        sync_create_and_connect(*it);
                        break;
                    }
                    case FTDGTask:
                    {
                        task_create_and_connect(*it, inner_relevant_vars, loops_ids);
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
            c = const_value_add(c, current_loop_info->_incr.get_constant());
            cn = NBase(const_value_to_nodecl(c));
        }
        loops_ids.pop_back();
        if (TDG_DEBUG)
            std::cerr << "**************** END expanding loop ****************" << std::endl;
    }

    void ExpandedTaskDependencyGraph::expand_condition(
            FTDGNode* n,
            std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> current_relevant_vars,
            std::deque<unsigned>& loops_ids)
    {
        if (TDG_DEBUG)
            std::cerr << "**************** Expanding condition ****************" << std::endl;

        Node* pcfg_n = n->get_pcfg_node();

        // Evaluate the condition
        Node* cond_node = pcfg_n->get_condition_node();
        NBase cond = get_condition_stmts(cond_node);
        ReplaceAndEvalVisitor rev(/*lhs*/ current_relevant_vars, /*rhs*/ current_relevant_vars);
        bool res = rev.walk(cond.shallow_copy());
        if (TDG_DEBUG)
            std::cerr << "        (ifelse " << pcfg_n->get_id() << ") codition : "
                      << cond.prettyprint() << " evaluates to " << res << std::endl;

        // If the condition evaluates to true, then create tasks within the true branch
        // otherwise create tasks within the false edge
        ObjectList<FTDGNode*> inner;
        std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> fixed_relevant_vars = current_relevant_vars;
        std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less> variable_relevant_vars;
        if (res)
        {
            inner = n->get_inner_true();
            store_dependency_relevant_vars(n, pcfg_n, /*true_edge*/true,
                                           fixed_relevant_vars, variable_relevant_vars);
        }
        else
        {
            inner = n->get_inner_false();
            store_dependency_relevant_vars(n, pcfg_n, /*true_edge*/false,
                                           fixed_relevant_vars, variable_relevant_vars);
        }
        if (TDG_DEBUG)
            print_relevant_vars(fixed_relevant_vars, variable_relevant_vars);

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
                    expand_loop(*it, fixed_relevant_vars, loops_ids);
                    break;
                }
                case FTDGCondition:
                {
                    expand_condition(*it, fixed_relevant_vars, loops_ids);
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
                    sync_create_and_connect(*it);
                    break;
                }
                case FTDGTask:
                {
                    task_create_and_connect(*it, fixed_relevant_vars, loops_ids);
                    break;
                }
                default:
                {
                    internal_error("Unexpected FTDGNode type %d.\n", it_type);
                }
            };
        }

        if (TDG_DEBUG)
            std::cerr << "************** END expanding condition **************" << std::endl;
    }

    void ExpandedTaskDependencyGraph::connect_nodes(ETDGNode* source, ETDGNode* target)
    {
        source->add_output(target);
        target->add_input(source);
        if (TDG_DEBUG)
            std::cerr << "        connecting " << source->get_id() << " -> " << target->get_id() << std::endl;
    }

    void ExpandedTaskDependencyGraph::disconnect_nodes(ETDGNode* source, ETDGNode* target)
    {
        source->remove_output(target);
        target->remove_input(source);
        if (TDG_DEBUG)
            std::cerr << "        disconnecting " << source->get_id() << " -> " << target->get_id() << std::endl;
    }

    ETDGNode* ExpandedTaskDependencyGraph::create_task_node(FTDGNode* ftdg_n, std::deque<unsigned> loops_ids)
    {
        ERROR_CONDITION(ftdg_n->get_type() != FTDGTask,
                        "Unsuported type %d for an ETDGNode. Only tasks accepted\n",
                        ftdg_n->get_type());

        Nodecl::NodeclBase source_task = ftdg_n->get_pcfg_node()->get_graph_related_ast();
        ERROR_CONDITION(!source_task.is<Nodecl::OpenMP::Task>(),
                        "The extensible graph node %d related with an ETDG task node has wrong type %s",
                        ftdg_n->get_pcfg_node()->get_id(),
                        ast_print_node_type(source_task.get_kind()));
        ETDGNode* etdg_n = new ETDGNode(get_etdg_node_id(ftdgnode_to_task_id.find(ftdg_n)->second, loops_ids),
                                        source_task);
        _source_to_etdg_nodes[source_task].append(etdg_n);

        if (TDG_DEBUG)
            std::cerr << "    task node " << etdg_n->get_id() << " with related pcfg node " << ftdg_n->get_pcfg_node()->get_id() << std::endl;

        _tasks.append(etdg_n);
        _leafs.insert(etdg_n);

        return etdg_n;
    }

    unsigned ExpandedTaskDependencyGraph::get_etdg_node_id(unsigned task_id, std::deque<unsigned> loops_ids)
    {
        if (TDG_DEBUG)
            std::cerr << "TASK " << task_id << "(";
        unsigned sum = 0;
        while (!loops_ids.empty())
        {
            if (TDG_DEBUG)
                std::cerr << loops_ids.back() << ", ";
            sum = (sum + loops_ids.back()) * _maxI;
            loops_ids.pop_back();
        }
        if (TDG_DEBUG)
            std::cerr << ")  ->  " << task_id + (_maxT * sum) << std::endl;
        return task_id + (_maxT * sum);
    }

    void ExpandedTaskDependencyGraph::connect_task_node(ETDGNode* etdg_n, Node* pcfg_n)
    {
        const ObjectList<Edge*>& pcfg_entries = pcfg_n->get_entry_edges();
        std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> etdg_n_vars_map = etdg_n->get_vars_map();
        for(ObjectList<Edge*>::const_iterator it = pcfg_entries.begin();
            it != pcfg_entries.end(); ++it)
        {
            // Skip the task creation
            if ((*it)->get_source()->is_omp_task_creation_node())
                continue;

            std::map<Node*, std::vector<ETDGNode*> >::iterator itm = pcfg_to_etdg_nodes.find((*it)->get_source());
            // Its parents have not been created yet
            if (itm == pcfg_to_etdg_nodes.end())
                continue;
            NBase cond = (*it)->get_condition();
            std::vector<ETDGNode*> etdg_nodes = itm->second;
            for (std::vector<ETDGNode*>::iterator itn = etdg_nodes.begin();
                 itn != etdg_nodes.end(); ++itn)
            {
                // Do not connect a ETDG node with itself
                if (etdg_n == *itn)
                    continue;

                // Replace variables with the corresponding constant values in the dependency expression
                bool res;
                if (cond.is_null())
                {
                    res = true;
                }
                else
                {
                    std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> itn_vars_map = (*itn)->get_vars_map();
                    ReplaceAndEvalVisitor rev(/*lhs*/ itn_vars_map, /*rhs*/ etdg_n_vars_map);
                    NBase cond_cp = cond.shallow_copy();
                    res = rev.walk(cond_cp);
                    nodecl_free(cond_cp.get_internal_nodecl());
                }
//                 if (TDG_DEBUG)
//                     std::cerr << "        (source " << (*itn)->get_id() << ") condition : "
//                               << (cond.is_null() ? "true" : cond.prettyprint())
//                               << " evaluates to " << res << std::endl;

                // If the condition evaluates to true, then connect the nodes
                if (res)
                {
                    connect_nodes(*itn, etdg_n);
                    _leafs.erase(*itn);
                }
            }
        }

        if (etdg_n->get_inputs().empty())
        {
            if (last_synchronization == NULL)
            {
                // The only entry node is the creation of the task
                if (TDG_DEBUG)
                    std::cerr << "         Insert " << etdg_n->get_id() << " in the list of roots" << std::endl;
                _roots.insert(etdg_n);
            }
            else
            {
                // FIXME This may not work if there are nested tasks and
                // last_synchronization comes from a taskwait node
                connect_nodes(last_synchronization, etdg_n);
            }
        }
    }

    void ExpandedTaskDependencyGraph::remove_task_transitive_inputs(ETDGNode* n)
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
                    disconnect_nodes(*its, n);
                }
            }
        }
    }

    void ExpandedTaskDependencyGraph::task_create_and_connect(
            FTDGNode* ftdg_n,
            std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> current_relevant_vars,
            std::deque<unsigned> loops_ids)
    {
        ETDGNode* etdg_n = create_task_node(ftdg_n, loops_ids);
        etdg_n->set_vars_map(current_relevant_vars);

        Node* pcfg_n = ftdg_n->get_pcfg_node();
        pcfg_to_etdg_nodes[pcfg_n].push_back(etdg_n);
        connect_task_node(etdg_n, pcfg_n);

        remove_task_transitive_inputs(etdg_n);
    }

    static int sync_id = -1;
    ETDGNode* ExpandedTaskDependencyGraph::create_sync_node(FTDGNode* ftdg_n)
    {
        ERROR_CONDITION(ftdg_n->get_type() != FTDGTaskwait && ftdg_n->get_type() != FTDGBarrier,
                        "Unsuported type %d for an ETDGNode. Only tasks accepted\n",
                        ftdg_n->get_type());
        if (TDG_DEBUG)
            std::cerr << "    sync node " << sync_id << " with related pcfg node " << ftdg_n->get_pcfg_node()->get_id() << std::endl;
        return new ETDGNode(sync_id--);
    }

    void ExpandedTaskDependencyGraph::connect_sync_node(ETDGNode* etdg_n)
    {
        for (std::set<ETDGNode*>::iterator it = _leafs.begin(); it != _leafs.end(); ++it)
        {
            connect_nodes(*it, etdg_n);
        }
        _leafs.clear();
    }

    void ExpandedTaskDependencyGraph::sync_create_and_connect(
            FTDGNode* ftdg_n)
    {
        ETDGNode* etdg_n = create_sync_node(ftdg_n);

        pcfg_to_etdg_nodes[ftdg_n->get_pcfg_node()].push_back(etdg_n);

        connect_sync_node(etdg_n);

        last_synchronization = etdg_n;
    }

    bool ExpandedTaskDependencyGraph::is_ancestor(ETDGNode* source, ETDGNode* target)
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

    void ExpandedTaskDependencyGraph::remove_barrier_nodes_rec(ETDGNode* n)
    {
        if (n->is_visited())
            return;

        n->set_visited(true);

        if (n->get_id() < 0)
        {
            // Disconnect the node from its parents and children and remove it
            const std::set<ETDGNode*>& inputs = n->get_inputs();
            for (std::set<ETDGNode*>::const_iterator it = inputs.begin(); it != inputs.end(); ++it)
            {
                disconnect_nodes(*it, n);
            }
            const std::set<ETDGNode*>& outputs = n->get_outputs();
            for (std::set<ETDGNode*>::const_iterator it = outputs.begin(); it != outputs.end(); ++it)
            {
                disconnect_nodes(n, *it);
            }
            delete n;

            if (outputs.empty())
            {
                // Restore the list of leafs
                for (std::set<ETDGNode*>::const_iterator it = inputs.begin(); it != inputs.end(); ++it)
                {
                    _leafs.insert(*it);
                }
            }
            else
            {
                // Connect all parents with all children
                for (std::set<ETDGNode*>::const_iterator its = inputs.begin(); its != inputs.end(); ++its)
                {
                    for (std::set<ETDGNode*>::const_iterator itt = outputs.begin(); itt != outputs.end(); ++itt)
                    {
                        connect_nodes(*its, *itt);
                    }
                }
            }
        }

        const std::set<ETDGNode*>& outputs = n->get_outputs();
        for (std::set<ETDGNode*>::const_iterator it = outputs.begin(); it != outputs.end(); ++it)
            remove_barrier_nodes_rec(*it);
    }

    void ExpandedTaskDependencyGraph::remove_barrier_nodes()
    {
        for (ObjectList<ETDGNode*>::iterator it = _roots.begin(); it != _roots.end(); ++it)
            remove_barrier_nodes_rec(*it);
    }

    void ExpandedTaskDependencyGraph::purge_etdg()
    {
        if (TDG_DEBUG)
            std::cerr << "Remove barrier nodes" << std::endl;
        remove_barrier_nodes();
        clear_visits();
    }

    void ExpandedTaskDependencyGraph::clear_visits_rec(ETDGNode* n)
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

    void ExpandedTaskDependencyGraph::clear_visits()
    {
        for (ObjectList<ETDGNode*>::iterator it = _roots.begin(); it != _roots.end(); ++it)
        {
            clear_visits_rec(*it);
        }
    }

    FlowTaskDependencyGraph* ExpandedTaskDependencyGraph::get_ftdg() const
    {
        return _ftdg;
    }

    unsigned ExpandedTaskDependencyGraph::get_maxI() const
    {
        return _maxI;
    }

    unsigned ExpandedTaskDependencyGraph::get_maxT() const
    {
        return _maxT;
    }

    unsigned ExpandedTaskDependencyGraph::get_nTasks() const
    {
        return _tasks.size();
    }

    ObjectList<ETDGNode*> ExpandedTaskDependencyGraph::get_tasks() const
    {
        return _tasks;
    }

    std::map<Nodecl::NodeclBase, ObjectList<ETDGNode*>,
             Nodecl::Utils::Nodecl_structural_less>
             ExpandedTaskDependencyGraph::get_source_to_etdg_nodes() const
    {
        return _source_to_etdg_nodes;
    }

    ObjectList<ETDGNode*> ExpandedTaskDependencyGraph::get_roots() const
    {
        return _roots;
    }

    std::set<ETDGNode*> ExpandedTaskDependencyGraph::get_leafs() const
    {
        return _leafs;
    }

}
}