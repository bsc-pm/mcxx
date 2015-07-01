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

#include <utility>

#include "cxx-cexpr.h"
#include "codegen-common.hpp"
#include "tl-expression-reduction.hpp"
#include "tl-iv-analysis.hpp"
#include "tl-use-def.hpp"

namespace TL {
namespace Analysis {

    // ********************************************************************************************* //
    // ******************************************* Utils ******************************************* //

namespace {
    const_value_t* one = const_value_get_one(/* bytes */ 4, /* signed */ 1);

    //!Returns true if a given nodecl is not modified in a given context
    bool is_constant_in_context(Node* ctx, const NBase& c)
    {
        // 1.- Base case
        if (c.is_constant())
            return true;

        // 2.- We sometimes call this function from an array subscript, which may be a Nodecl::List
        // Thus, to unify all cases, we always consider the rest of this method as if we had a list
        const Nodecl::List& cs = (c.is<Nodecl::List>() ? c.as<Nodecl::List>() : Nodecl::List::make(c.shallow_copy()));

        // 3.- It may happen that the usage information has not been synthesized for a given node
        // In that case, we have to do it here
        if (!ctx->has_key(_UPPER_EXPOSED) && !ctx->has_key(_KILLED) && !ctx->has_key(_UNDEF))
        {
            set_graph_node_use_def(ctx);
            ExtensibleGraph::clear_visits_extgraph(ctx);
        }

        // 4.- Finally check whether \param c is modified in the context or not
        for (Nodecl::List::const_iterator it = cs.begin(); it != cs.end(); ++it)
        {
            if (!it->is_constant())
            {
                const NodeclList& memory_accesses = Nodecl::Utils::get_all_memory_accesses(*it);
                for (NodeclList::const_iterator itm = memory_accesses.begin();
                     itm != memory_accesses.end(); ++itm)
                {
                    if (Utils::nodecl_set_contains_nodecl(*itm, ctx->get_killed_vars())
                        || Utils::nodecl_set_contains_nodecl(*itm, ctx->get_undefined_behaviour_vars()))
                    {
                        return false;
                    }
                }
            }
        }

        return true;
    }

    // FIXME We have to add the family as a reference parameter for the derived induction variables
    bool is_accepted_induction_variable_syntax(Node* loop, NBase stmt, NBase& iv, NBase& incr)
    {
        bool is_iv = false;

        if (stmt.is<Nodecl::Assignment>())
        {
            Nodecl::Assignment st_ = stmt.as<Nodecl::Assignment>();
            NBase lhs = st_.get_lhs();
            NBase rhs = st_.get_rhs();

            if (rhs.is<Nodecl::Add>())
            {   // Expression accepted: iv = c + iv; iv = iv + x; iv = -c + iv; iv = iv - x;
                Nodecl::Add _rhs = rhs.as<Nodecl::Add>();
                const NBase& rhs_lhs = _rhs.get_lhs();
                const NBase& rhs_rhs = _rhs.get_rhs();

                // The LHS is accepted to be an array subscript only if the subscripts are constant in the loop
                if ((!lhs.is<Nodecl::ArraySubscript>()
                        || is_constant_in_context(loop, lhs.as<Nodecl::ArraySubscript>().get_subscripts())))
                {
                    if (Nodecl::Utils::structurally_equal_nodecls(lhs, rhs_rhs, /*skip_conversions*/true)
                            && is_constant_in_context(loop, rhs_lhs))
                    {   // iv = c + iv
                        iv = lhs;
                        incr = rhs_lhs;
                        is_iv = true;
                    }
                    else if (Nodecl::Utils::structurally_equal_nodecls(lhs, rhs_lhs, /*skip_conversions*/true)
                            && is_constant_in_context(loop, rhs_rhs))
                    {   // iv = iv + c
                        iv = lhs;
                        incr = rhs_rhs;
                        is_iv = true;
                    }
                    else if (loop->is_loop_induction_variable(rhs_lhs)
                            && is_constant_in_context(loop, rhs_rhs))
                    {   // iv = iv_B + x
                        iv = lhs;
                        incr = rhs_rhs;
                        is_iv = true;
                    }
                    else if (loop->is_loop_induction_variable(rhs_rhs)
                            && is_constant_in_context(loop, rhs_lhs))
                    {   // iv = x + iv_B
                        iv = lhs;
                        incr = rhs_lhs;
                        is_iv = true;
                    }
                }
            }
        }
        else if (stmt.is<Nodecl::AddAssignment>())
        {   // Expression accepted: iv += x;
            Nodecl::AddAssignment st_ = stmt.as<Nodecl::AddAssignment>();
            const NBase& lhs = st_.get_lhs();
            const NBase& rhs = st_.get_rhs();
            if (is_constant_in_context(loop, rhs)
                    && (!lhs.is<Nodecl::ArraySubscript>()
                            || (lhs.is<Nodecl::ArraySubscript>()
                                && is_constant_in_context(loop, lhs.as<Nodecl::ArraySubscript>().get_subscripts()))))
            {
                iv = st_.get_lhs();
                incr = st_.get_rhs();
                is_iv = true;
            }
        }
        else if (stmt.is<Nodecl::MinusAssignment>())
        {   // Expression accepted: iv -= x;
            Nodecl::MinusAssignment st_ = stmt.as<Nodecl::MinusAssignment>();
            NBase lhs = st_.get_lhs();
            NBase rhs = st_.get_rhs();
            if (is_constant_in_context(loop, rhs)
                    && (!lhs.is<Nodecl::ArraySubscript>()
                            || (lhs.is<Nodecl::ArraySubscript>()
                                && is_constant_in_context(loop, lhs.as<Nodecl::ArraySubscript>().get_subscripts()))))
            {
                NBase new_rhs = Nodecl::Neg::make(rhs.shallow_copy(), rhs.get_type(), rhs.get_locus());
                iv = st_.get_lhs();
                incr = new_rhs;
                is_iv = true;
            }
        }
        else if (stmt.is<Nodecl::Preincrement>())
        {
            NBase rhs = stmt.as<Nodecl::Preincrement>().get_rhs();
            iv = rhs;
            incr = Nodecl::IntegerLiteral::make(rhs.get_type(), one);
            is_iv = true;
        }
        else if (stmt.is<Nodecl::Postincrement>())
        {
            NBase rhs = stmt.as<Nodecl::Postincrement>().get_rhs();
            iv = rhs;
            incr = Nodecl::IntegerLiteral::make(rhs.get_type(), one);
            is_iv = true;
        }
        else if (stmt.is<Nodecl::Predecrement>())
        {
            NBase rhs = stmt.as<Nodecl::Predecrement>().get_rhs();
            iv = rhs;
            incr = Nodecl::Neg::make(const_value_to_nodecl(one), rhs.get_type());
            incr.set_constant(const_value_neg(one));
            is_iv = true;
        }
        else if (stmt.is<Nodecl::Postdecrement>())
        {
            NBase rhs = stmt.as<Nodecl::Postdecrement>().get_rhs();
            iv = rhs;
            incr = Nodecl::Neg::make(const_value_to_nodecl(one), rhs.get_type());
            incr.set_constant(const_value_neg(one));
            is_iv = true;
        }

        // Purge the results: if the variable detected as a possible IV is declared within the loop context,
        // then it cannot be a IV of the loop!
        if (!iv.is_null())
        {
            Scope loop_ctx = loop->get_graph_related_ast().retrieve_context();
            Scope iv_ctx = Utils::get_nodecl_base(iv).get_symbol().get_scope();
            if (iv_ctx.scope_is_enclosed_by(loop_ctx))
            {
                iv = Nodecl::NodeclBase::null();
                incr = Nodecl::NodeclBase::null();
                is_iv = false;
            }
        }

        return is_iv;
    }

}

    // ***************************************** END Utils ***************************************** //
    // ********************************************************************************************* //



    // ********************************************************************************************* //
    // ************************** Class for induction variables analysis *************************** //

    InductionVariableAnalysis::InductionVariableAnalysis(ExtensibleGraph* graph)
            : _induction_vars(), _graph(graph), _var_type(Utils::BASIC_IV), _non_induction_vars()
    {}

    void InductionVariableAnalysis::compute_induction_variables()
    {
        Node* graph = _graph->get_graph();
        bool changed = true;
        while (changed)
        {
            changed = false;
            compute_induction_variables_rec(graph, changed);
            ExtensibleGraph::clear_visits(graph);
            _var_type = Utils::DERIVED_IV;  // In the first iteration, the IVs are BASIC,
                                            // but in the following, we can only found DERIVED
        }
    }

    void InductionVariableAnalysis::compute_induction_variables_rec(Node* n, bool& changed)
    {
        if (n->is_visited())
            return;

        n->set_visited(true);

        if (n->is_graph_node())
        {
            // IVs are computed from inner to outer loops
            Node* entry = n->get_graph_entry_node();
            compute_induction_variables_rec(entry, changed);

            if (n->is_loop_node())
            {   // Treat current loop
                ExtensibleGraph::clear_visits_in_level(entry, n);
                compute_loop_induction_variables(entry, n, changed);
            }
            else if (n->is_omp_loop_node())
            {   //Propagate induction variables from the inner loop to the omp loop node
                Node* loop_entry_node = n->get_graph_entry_node()->get_children()[0];
                Node* loop_node = loop_entry_node->get_children()[0];
                std::pair<Utils::InductionVarsPerNode::iterator, Utils::InductionVarsPerNode::iterator> loop_ivs =
                _induction_vars.equal_range(loop_node->get_id());
                for (Utils::InductionVarsPerNode::iterator it = loop_ivs.first; it != loop_ivs.second; ++it)
                {
                    n->set_induction_variable(it->second);
                    _induction_vars.insert(std::pair<int, Utils::InductionVar*>(n->get_id(), it->second));
                }
            }
        }

        const ObjectList<Node*>& children = n->get_children();
        for (ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
            compute_induction_variables_rec(*it, changed);
    }

    void InductionVariableAnalysis::compute_loop_induction_variables(Node* n, Node* loop, bool& changed)
    {
        if (n->is_visited()
                || !n->node_is_enclosed_by(loop)) // Avoid exiting the scope (return and goto may cause it)
            return;

        n->set_visited(true);

        if (n->is_graph_node())
        {
            compute_loop_induction_variables(n->get_graph_entry_node(), loop, changed);
        }
        else
        {   // Look for IVs in the current node
            const NodeclList& stmts = n->get_statements();
            for (NodeclList::const_iterator it = stmts.begin(); it != stmts.end(); ++it)
            {
                NBase incr;
                ObjectList<NBase> incr_list;
                NBase iv = is_induction_variable(*it, loop, incr, incr_list);
                if (!iv.is_null())
                {
                    changed = true;
                    Utils::InductionVar* ivd = new Utils::InductionVar(iv, _var_type, iv);
                    ivd->set_increment(incr);
                    ivd->set_increment_list(incr_list);
                    loop->set_induction_variable(ivd);
                    _induction_vars.insert(std::pair<int, Utils::InductionVar*>(loop->get_id(), ivd));
                }
            }
        }

        // Look for IVs in current's children
        const ObjectList<Node*>& children = n->get_children();
        for(ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
            compute_loop_induction_variables(*it, loop, changed);
    }

    // FIXME This method does not cover all kind induction variable.
    // F.i., 'st': iv = 1 + iv + z, where 'z' is loop invariant, will return false
    NBase InductionVariableAnalysis::is_induction_variable(
            const NBase& st, Node* loop, NBase& incr, ObjectList<NBase>& incr_list)
    {
        NBase iv = NBase::null();

        if (is_accepted_induction_variable_syntax(loop, st, iv, incr))
        {
            // Check if this IV has already been found as IV
            std::pair<Utils::InductionVarsPerNode::iterator, Utils::InductionVarsPerNode::iterator> loop_ivs =
                    _induction_vars.equal_range(loop->get_id());
            for (Utils::InductionVarsPerNode::iterator it = loop_ivs.first; it != loop_ivs.second; ++it)
            {
                if (Nodecl::Utils::structurally_equal_nodecls(it->second->get_variable(), iv, /*skip_conversions*/true))
                    return NBase::null();   // The variable was already detected as an IV, there is nothing else to do
            }

            // Check if this IV has already been found as non-IV
            std::pair<std::map<int, Nodecl::NodeclBase>::iterator, std::map<int, Nodecl::NodeclBase>::iterator> loop_non_ivs =
                    _non_induction_vars.equal_range(loop->get_id());
            for (std::map<int, Nodecl::NodeclBase>::iterator it = loop_non_ivs.first; it != loop_non_ivs.second; ++it)
            {
                if (Nodecl::Utils::structurally_equal_nodecls(it->second, iv, /*skip_conversions*/true))
                    return NBase::null();   // The variable was already detected as an IV, there is nothing else to do
            }

            // Check if the variable is modified in some other point of the loop
            if (!check_potential_induction_variable(iv, incr, incr_list, st, loop))
            {   // The variable is not really an IV => return a null nodecl
                _non_induction_vars.insert(std::pair<int, Nodecl::NodeclBase>(loop->get_id(), iv));
                return NBase::null();
            }
            else
            {   // The variable is an IV => add the increment to the list of increments of the variable
                incr_list.insert(incr);
            }
        }

        return iv;
    }

    bool InductionVariableAnalysis::check_potential_induction_variable(
            const NBase& iv, NBase& incr, NodeclList& incr_list,
            const NBase& stmt, Node* loop)
    {
        // Check whether the variable is modified in other places inside the loop
        Node* entry = loop->get_graph_entry_node();
        bool res = check_undesired_modifications(iv, incr, incr_list, stmt, entry, loop);
        ExtensibleGraph::clear_visits_aux(entry);
        if (res)    // If the possible IV has undesired modifications, then return false because it is not an IV
            return false;

        // Check whether the variable is always the same memory location (avoid things like a[b[0]]++)
        res = check_constant_memory_access(iv, loop);
        ExtensibleGraph::clear_visits_aux(loop);

        return res;
    }

    bool InductionVariableAnalysis::check_undesired_modifications(
            const NBase& iv, NBase& incr, NodeclList& incr_list,
            const NBase& stmt, Node* node, Node* loop)
    {
        if (node->is_visited_aux())
            return false;

        if (node->get_id() == loop->get_graph_exit_node()->get_id())
            return false;

        node->set_visited_aux(true);

        if (node->is_graph_node())
        {
            if (check_undesired_modifications(iv, incr, incr_list, stmt, node->get_graph_entry_node(), loop))
                return true;
        }
        else if (!node->is_exit_node())
        {
            // Check the current node
            const NodeclList& stmts = node->get_statements();
            FalseInductionVariablesVisitor v(iv, &incr, &incr_list, loop);
            for (NodeclList::const_iterator it = stmts.begin(); it != stmts.end(); ++it)
            {
                // Check the statement only if it is not the statement where the potential IV was found
                if (!Nodecl::Utils::structurally_equal_nodecls(stmt, *it, /*skip_conversions*/true))
                {
                    v.walk(*it);
                    if (!v.get_is_induction_variable())
                    {
                        return true;
                    }
                }
            }
        }

        // If IV still looks like an IV, check for false positives in the children nodes
        const ObjectList<Node*>& children = node->get_children();
        for (ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
        {
            if (check_undesired_modifications(iv, incr, incr_list, stmt, *it, loop))
            {
                return true;
            }
        }

        return false;
    }

    bool InductionVariableAnalysis::check_constant_memory_access(const NBase& iv, Node* loop)
    {
        if (iv.is<Nodecl::Symbol>() || iv.is<Nodecl::ClassMemberAccess>())
        {}      // Nothing to be done: this will always be the same memory location
        else if (iv.is<Nodecl::ArraySubscript>())
        {
            const Nodecl::ArraySubscript& iv_as = iv.as<Nodecl::ArraySubscript>();
            const Nodecl::List& subscripts = iv_as.get_subscripts().as<Nodecl::List>();
            for (Nodecl::List::const_iterator it = subscripts.begin(); it != subscripts.end(); ++it)
            {
                if (!is_constant_in_context(loop, *it))
                    return false;
            }
        }
        else if (iv.is<Nodecl::Dereference>())
        {
            WARNING_MESSAGE("Dereference as Induction Variables analysis is not yet supported\n", 0);
        }
        else
        {
            WARNING_MESSAGE ("Unexpected type of node '%s' as Induction Variable\n", ast_print_node_type(iv.get_kind()));
        }

        return true;
    }

    Utils::InductionVarsPerNode InductionVariableAnalysis::get_all_induction_vars() const
    {
        return _induction_vars;
    }

    // ************************ END class for induction variables analysis ************************* //
    // ********************************************************************************************* //



    // ********************************************************************************************* //
    // ****************** Visitor that checks whether a potential IV is a real IV ****************** //

    FalseInductionVariablesVisitor::FalseInductionVariablesVisitor(
            NBase iv, NBase* incr, NodeclList* incr_list, Node* loop)
        : _iv(iv), _incr(incr), _incr_list(incr_list),  _loop(loop),
          _is_induction_var(true), _n_nested_conditionals(0), _calc()
    {}

    bool FalseInductionVariablesVisitor::get_is_induction_variable() const
    {
        return _is_induction_var;
    }

    void FalseInductionVariablesVisitor::undefine_induction_variable()
    {
        _is_induction_var = false;
        _incr = NULL;
        _incr_list->empty();
    }

    void FalseInductionVariablesVisitor::join_list(TL::ObjectList<bool>& list)
    {   // nothing to be done
    }

    void FalseInductionVariablesVisitor::unhandled_node(const NBase& n)
    {
        std::cerr << "Unhandled node while Induction Variable analysis '"
                  << n.prettyprint() << "' of type '" << ast_print_node_type(n.get_kind()) << "'" << std::endl;
    }

    static bool are_contained_one_in_another(const NBase& a, const NBase& b)
    {
        return (Nodecl::Utils::dataref_contains_dataref(a, b)
                    || Nodecl::Utils::dataref_contains_dataref(b, a));
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::AddAssignment& n)
    {
        if (!are_contained_one_in_another(n.get_lhs(), _iv))
            return;

        if (_n_nested_conditionals > 0)
        {
            // An induction variable may never be modified inside a conditional block
            undefine_induction_variable();
        }
        else
        {
            NBase new_incr;
            // Check whether the statement has the accepted syntax of an induction variable
            if (is_accepted_induction_variable_syntax(_loop, n, _iv, new_incr))
            {
                if (_incr->is_constant() && new_incr.is_constant())
                {
                    // Check if the increments are linear and can be combined
                    if (const_value_is_positive(_incr->get_constant()) &&
                        const_value_is_positive(new_incr.get_constant()))
                    {
                        _incr_list->insert(new_incr);
                        NBase c = Nodecl::Add::make(*_incr, new_incr, _incr->get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        *_incr = const_value_to_nodecl(c_value);
                    }
                    else if (const_value_is_negative(_incr->get_constant()) &&
                        const_value_is_negative(new_incr.get_constant()))
                    {
                        _incr_list->insert(new_incr);
                        NBase c = Nodecl::Minus::make(*_incr, new_incr, _incr->get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        *_incr = const_value_to_nodecl(c_value);
                    }
                    else
                    {
                        undefine_induction_variable();
                    }
                }
                else
                {
                    undefine_induction_variable();
                }
            }
        }
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::ArithmeticShrAssignment& n)
    {
        if (are_contained_one_in_another(n.get_lhs(), _iv))
            undefine_induction_variable();
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::Assignment& n)
    {
        if (!are_contained_one_in_another(n.get_lhs(), _iv))
            return;

        if (_n_nested_conditionals > 0)
        {
            // An induction variable may never be modified inside a conditional block
            undefine_induction_variable();
        }
        else
        {
            NBase new_incr;
            // Check whether the statement has the accepted syntax of an induction variable
            if (is_accepted_induction_variable_syntax(_loop, n, _iv, new_incr))
            {
                if (_incr->is_constant() && new_incr.is_constant())
                {
                    // Check if the increments are linear and can be combined
                    if (const_value_is_positive(_incr->get_constant()) &&
                        const_value_is_positive(new_incr.get_constant()))
                    {
                        _incr_list->insert(new_incr);
                        NBase c = Nodecl::Add::make(*_incr, new_incr, _incr->get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        *_incr = const_value_to_nodecl(c_value);
                    }
                    else if (const_value_is_negative(_incr->get_constant()) &&
                        const_value_is_negative(new_incr.get_constant()))
                    {
                        _incr_list->insert(new_incr);
                        NBase c = Nodecl::Minus::make(*_incr, new_incr, _incr->get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        *_incr = const_value_to_nodecl(c_value);
                    }
                    else
                    {
                        undefine_induction_variable();
                    }
                }
                else
                {
                    undefine_induction_variable();
                }
            }
            else
            {
                undefine_induction_variable();
            }
        }
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::BitwiseAndAssignment& n)
    {
        if (are_contained_one_in_another(n.get_lhs(), _iv))
            undefine_induction_variable();
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::BitwiseOrAssignment& n)
    {
        if (are_contained_one_in_another(n.get_lhs(), _iv))
            undefine_induction_variable();
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::BitwiseShlAssignment& n)
    {
        if (are_contained_one_in_another(n.get_lhs(), _iv))
            undefine_induction_variable();
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::BitwiseShrAssignment& n)
    {
        if (are_contained_one_in_another(n.get_lhs(), _iv))
            undefine_induction_variable();
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::BitwiseXorAssignment& n)
    {
        if (are_contained_one_in_another(n.get_lhs(), _iv))
            undefine_induction_variable();
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::ConditionalExpression& n)
    {
        walk(n.get_condition());
        _n_nested_conditionals++;
        walk(n.get_true());
        walk(n.get_false());
        _n_nested_conditionals--;
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::DivAssignment& n)
    {
        if (are_contained_one_in_another(n.get_lhs(), _iv))
            undefine_induction_variable();
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::IfElseStatement& n)
    {
        walk(n.get_condition());
        _n_nested_conditionals++;
        walk(n.get_then());
        walk(n.get_else());
        _n_nested_conditionals--;
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::MinusAssignment& n)
    {
        if (!are_contained_one_in_another(n.get_lhs(), _iv))
            return;

        if (_n_nested_conditionals > 0)
        {
            // An induction variable may never be modified inside a conditional block
            undefine_induction_variable();
        }
        else
        {
            NBase new_incr;
            // Check whether the statement has the accepted syntax of an induction variable
            if (is_accepted_induction_variable_syntax(_loop, n, _iv, new_incr))
            {
                if (_incr->is_constant() && new_incr.is_constant())
                {
                    // Check if the increments are linear and can be combined
                    if (const_value_is_positive(_incr->get_constant()) &&
                        const_value_is_positive(new_incr.get_constant()))
                    {
                        _incr_list->insert(new_incr);
                        NBase c = Nodecl::Add::make(*_incr, new_incr, _incr->get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        *_incr = const_value_to_nodecl(c_value);
                    }
                    else if (const_value_is_negative(_incr->get_constant()) &&
                        const_value_is_negative(new_incr.get_constant()))
                    {
                        _incr_list->insert(new_incr);
                        NBase c = Nodecl::Minus::make(*_incr, new_incr, _incr->get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        *_incr = const_value_to_nodecl(c_value);
                    }
                    else
                    {
                        undefine_induction_variable();
                    }
                }
                else
                {
                    undefine_induction_variable();
                }
            }
        }
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::ModAssignment& n)
    {
        if (are_contained_one_in_another(n.get_lhs(), _iv))
            undefine_induction_variable();
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::MulAssignment& n)
    {
        if (are_contained_one_in_another(n.get_lhs(), _iv))
            undefine_induction_variable();
    }

    void FalseInductionVariablesVisitor::visit_decrement(const Nodecl::NodeclBase& n)
    {
        if (!are_contained_one_in_another(n, _iv))
            return;

        if (_n_nested_conditionals > 0)
        {
            // An induction variable may never be modified inside a conditional block
            undefine_induction_variable();
        }
        else
        {
            // Check if the increments are linear and can be combined
            if (_incr->is_constant() && const_value_is_negative(_incr->get_constant()))
            {
                NBase new_incr = const_value_to_nodecl(const_value_get_one(/*bytes*/ 4, /*signed*/ 1));
                NBase c = Nodecl::Minus::make(*_incr, new_incr, _incr->get_type());
                const_value_t* c_value = _calc.compute_const_value(c);
                *_incr = const_value_to_nodecl(c_value);
                _incr_list->insert(new_incr);
            }
            else
            {
                undefine_induction_variable();
            }
        }
    }

    void FalseInductionVariablesVisitor::visit_increment(const Nodecl::NodeclBase& n)
    {
        if (!are_contained_one_in_another(n, _iv))
            return;

        if (_n_nested_conditionals > 0)
        {
            // An induction variable may never be modified inside a conditional block
            undefine_induction_variable();
        }
        else
        {
            // Check if the increments are linear and can be combined
            if (_incr->is_constant() && const_value_is_positive(_incr->get_constant()))
            {
                NBase new_incr = const_value_to_nodecl(const_value_get_one(/*bytes*/ 4, /*signed*/ 1));
                NBase c = Nodecl::Add::make(*_incr, new_incr, _incr->get_type());
                const_value_t* c_value = _calc.compute_const_value(c);
                *_incr = const_value_to_nodecl(c_value);
                _incr_list->insert(new_incr);
            }
            else
            {
                undefine_induction_variable();
            }
        }
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::Postdecrement& n)
    {
        visit_decrement(n.get_rhs());
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::Postincrement& n)
    {
        visit_increment(n.get_rhs());
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::Predecrement& n)
    {
        visit_decrement(n.get_rhs());
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::Preincrement& n)
    {
        visit_increment(n.get_rhs());
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::ObjectInit& n)
    {
        Symbol n_sym = n.get_symbol();
        Symbol iv_sym = Utils::get_nodecl_base(_iv).get_symbol();
        if (n_sym == iv_sym)
            undefine_induction_variable();
    }

    // ************************ END class for induction variables analysis ************************* //
    // ********************************************************************************************* //
}
}
