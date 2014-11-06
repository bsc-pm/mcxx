/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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
#include "tl-node.hpp"

namespace TL {
namespace Analysis {

namespace {
    const_value_t* one = const_value_get_one(/* bytes */ 4, /* signed */ 1);

    bool is_accepted_induction_variable_syntax(Node* loop, NBase stmt, NBase& iv, NBase& incr)
    {
        bool is_iv = false;

        if(stmt.is<Nodecl::Assignment>())
        {
            Nodecl::Assignment st_ = stmt.as<Nodecl::Assignment>();
            NBase lhs = st_.get_lhs();
            NBase rhs = st_.get_rhs();

            NBase lhs_rhs, rhs_rhs;
            if(rhs.is<Nodecl::Add>())
            {   // Expression accepted: iv = c + iv; iv = iv + x; iv = -c + iv; iv = iv - x;
                Nodecl::Add _rhs = rhs.as<Nodecl::Add>();
                lhs_rhs = _rhs.get_lhs();
                rhs_rhs = _rhs.get_rhs();

                if((!lhs.is<Nodecl::ArraySubscript>() ||
                      (lhs.is<Nodecl::ArraySubscript>() &&
                        ExtensibleGraph::is_constant_in_context(loop, lhs.as<Nodecl::ArraySubscript>().get_subscripts()))))
                {
                    if(Nodecl::Utils::structurally_equal_nodecls(lhs, rhs_rhs, /*skip_conversion_node*/ true) &&
                        ExtensibleGraph::is_constant_in_context(loop, lhs_rhs))
                    {
                        iv = lhs;
                        incr = lhs_rhs;
                        is_iv = true;
                    }
                    else if(Nodecl::Utils::structurally_equal_nodecls(lhs, lhs_rhs, /*skip_conversion_node*/ true) &&
                        ExtensibleGraph::is_constant_in_context(loop, rhs_rhs))
                    {
                        iv = lhs;
                        incr = rhs_rhs;
                        is_iv = true;
                    }
                }
            }
        }
        else if(stmt.is<Nodecl::AddAssignment>())
        {   // Expression accepted: iv += x;
            Nodecl::AddAssignment st_ = stmt.as<Nodecl::AddAssignment>();
            NBase lhs = st_.get_lhs();
            if(ExtensibleGraph::is_constant_in_context(loop, st_.get_rhs())
                && (!lhs.is<Nodecl::ArraySubscript>()
                        || (lhs.is<Nodecl::ArraySubscript>()
                            && ExtensibleGraph::is_constant_in_context(loop, lhs.as<Nodecl::ArraySubscript>().get_subscripts()))))
            {
                iv = st_.get_lhs();
                incr = st_.get_rhs();
                is_iv = true;
            }
        }
        else if(stmt.is<Nodecl::MinusAssignment>())
        {   // Expression accepted: iv -= x;
            Nodecl::MinusAssignment st_ = stmt.as<Nodecl::MinusAssignment>();
            NBase lhs = st_.get_lhs();
            NBase rhs = st_.get_rhs();
            if(ExtensibleGraph::is_constant_in_context(loop, st_.get_rhs())
                && (!lhs.is<Nodecl::ArraySubscript>()
                        || (lhs.is<Nodecl::ArraySubscript>()
                            && ExtensibleGraph::is_constant_in_context(loop, lhs.as<Nodecl::ArraySubscript>().get_subscripts()))))
            {
                NBase new_rhs = Nodecl::Neg::make(rhs.shallow_copy(), rhs.get_type(), rhs.get_locus());
                iv = st_.get_lhs();
                incr = new_rhs;
                is_iv = true;
            }
        }
        else if(stmt.is<Nodecl::Preincrement>())
        {
            NBase rhs = stmt.as<Nodecl::Preincrement>().get_rhs();
            iv = rhs;
            incr = Nodecl::IntegerLiteral::make(rhs.get_type(), one);
            is_iv = true;
        }
        else if(stmt.is<Nodecl::Postincrement>())
        {
            NBase rhs = stmt.as<Nodecl::Postincrement>().get_rhs();
            iv = rhs;
            incr = Nodecl::IntegerLiteral::make(rhs.get_type(), one);
            is_iv = true;
        }
        else if(stmt.is<Nodecl::Predecrement>())
        {
            NBase rhs = stmt.as<Nodecl::Predecrement>().get_rhs();
            iv = rhs;
            incr = Nodecl::Neg::make(const_value_to_nodecl(one), rhs.get_type());
            incr.set_constant(const_value_neg(one));
            is_iv = true;
        }
        else if(stmt.is<Nodecl::Postdecrement>())
        {
            NBase rhs = stmt.as<Nodecl::Postdecrement>().get_rhs();
            iv = rhs;
            incr = Nodecl::Neg::make(const_value_to_nodecl(one), rhs.get_type());
            incr.set_constant(const_value_neg(one));
            is_iv = true;
        }

        return is_iv;
    }
}

    // ***************************************** END Utils ***************************************** //
    // ********************************************************************************************* //



    // ********************************************************************************************* //
    // ************************** Class for induction variables analysis *************************** //

    InductionVariableAnalysis::InductionVariableAnalysis(ExtensibleGraph* graph)
            : _induction_vars(), _graph(graph)
    {}

    void InductionVariableAnalysis::compute_induction_variables()
    {
        Node* graph = _graph->get_graph();
        compute_induction_variables_rec(graph);
        ExtensibleGraph::clear_visits(graph);
    }

    void InductionVariableAnalysis::compute_induction_variables_rec(Node* current)
    {
        if(!current->is_visited())
        {
            current->set_visited(true);

            if(current->is_graph_node())
            {
                // IV is computed from inner to outer loops
                Node* entry = current->get_graph_entry_node();
                compute_induction_variables_rec(entry);

                if(current->is_loop_node())
                {   // Treat current loop
                    ExtensibleGraph::clear_visits_in_level(entry, current);
                    detect_basic_induction_variables(entry, current);
//                     ExtensibleGraph::clear_visits_in_level(entry, current);
//                     detect_derived_induction_variables(entry, current);
                }
                else if(current->is_omp_loop_node())
                {   //Propagate induction variables from the inner loop to the omp loop node
                    Node* loop_entry_node = current->get_graph_entry_node()->get_children()[0];
                    Node* loop_node = loop_entry_node->get_children()[0];
                    std::pair<Utils::InductionVarsPerNode::iterator, Utils::InductionVarsPerNode::iterator> loop_ivs =
                    _induction_vars.equal_range(loop_node->get_id());
                    for(Utils::InductionVarsPerNode::iterator it = loop_ivs.first; it != loop_ivs.second; ++it)
                    {
                        current->set_induction_variable(it->second);
                        _induction_vars.insert(std::pair<int, Utils::InductionVar*>(current->get_id(), it->second));
                    }
                }
            }

            ObjectList<Node*> children = current->get_children();
            for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                compute_induction_variables_rec(*it);
        }
    }

    void InductionVariableAnalysis::detect_basic_induction_variables(Node* current, Node* loop)
    {
        if (current->is_visited() || current->is_graph_exit_node(loop))
            return;

        current->set_visited(true);

        if (current->is_graph_node() && !current->is_loop_node())
        {
            detect_basic_induction_variables(current->get_graph_entry_node(), loop);
        }
        else
        {
            // Look for IVs in the current node
            NodeclList stmts = current->get_statements();
            for (NodeclList::iterator it = stmts.begin(); it != stmts.end(); ++it)
            {
                NBase incr;
                ObjectList<NBase> incr_list;
                NBase iv = is_basic_induction_variable(*it, loop, incr, incr_list);
                if (!iv.is_null())
                {
                    Utils::InductionVar* ivd = new Utils::InductionVar(iv, Utils::BASIC_IV, iv);
                    ivd->set_increment(incr);
                    ivd->set_increment_list(incr_list);
                    loop->set_induction_variable(ivd);
                    _induction_vars.insert(std::pair<int, Utils::InductionVar*>(loop->get_id(), ivd));
                }
            }
        }

        // Look for IVs in current's children
        const ObjectList<Node*>& children = current->get_children();
        for(ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
            detect_basic_induction_variables(*it, loop);
    }

    // FIXME This method does not cover all kind induction variable.
    // F.i., 'st': iv = 1 + iv + z, where 'z' is loop invariant, will return false
    NBase InductionVariableAnalysis::is_basic_induction_variable(NBase st, Node* loop,
                                                                 NBase& incr, ObjectList<NBase>& incr_list)
    {
        NBase iv = NBase::null();

        if(is_accepted_induction_variable_syntax(loop, st, iv, incr))
        {
            incr_list.insert(incr);
            // Get a list from the IVs in the map corresponding to the current loop
            Utils::InductionVarList ivs;
            std::pair<Utils::InductionVarsPerNode::iterator, Utils::InductionVarsPerNode::iterator> loop_ivs =
                    _induction_vars.equal_range(loop->get_id());
            for(Utils::InductionVarsPerNode::iterator it = loop_ivs.first; it != loop_ivs.second; ++it)
                ivs.append(it->second);

            if(!Utils::induction_variable_list_contains_variable(ivs, iv))
            {
                if(!check_potential_induction_variable(iv, incr, incr_list, st, loop))
                    iv = NBase::null();
            }
            else
            {
                iv = NBase::null();
            }
        }

        return iv;
    }

    void InductionVariableAnalysis::detect_derived_induction_variables(Node* current, Node* loop)
    {
        if(!current->is_visited() && current->is_graph_exit_node(loop))
        {
            current->set_visited(true);

            // Look for IVs in the current node
            NodeclList stmts = current->get_statements();
            for(NodeclList::iterator it = stmts.begin(); it != stmts.end(); ++it)
            {
                NBase iv_family;
                NBase n = is_derived_induction_variable(*it, current, loop, iv_family);
                if(!n.is_null())
                {
                    Utils::InductionVar* iv = new Utils::InductionVar(n, Utils::DERIVED_IV, n);
                    loop->set_induction_variable(iv);
                    _induction_vars.insert(std::pair<int, Utils::InductionVar*>(loop->get_id(), iv));
                }

            }
        }
    }

    NBase InductionVariableAnalysis::is_derived_induction_variable(NBase st, Node* current,
                                                                   Node* loop, NBase& family)
    {
        NBase res = NBase::null();

//         Node* loop_entry = loop->get_graph_entry_node();
//         int id_end = loop->get_graph_exit_node()->get_id();
//         if(st.is<Nodecl::Assignment>())
//         {   /*! Expressions accepted
//                 * . iv_1 = iv_2 + x;
//                 * . iv_1 = x + iv_2;
//                 * . iv_1 = iv_2 * x;
//                 * . iv_1 = x * iv_2; */
//             Nodecl::Assignment _st = st.as<Nodecl::Assignment>();
//             NBase lhs = _st.get_lhs();
//             NBase rhs = _st.get_rhs();
//
//             NBase lhs_rhs, rhs_rhs;
//             if(rhs.is<Nodecl::Add>())
//             {
//                 Nodecl::Add rhs_ = rhs.as<Nodecl::Add>();
//                 lhs_rhs = rhs_.get_lhs();
//                 rhs_rhs = rhs_.get_rhs();
//             }
//             else if(rhs.is<Nodecl::Mul>())
//             {
//                 Nodecl::Mul rhs_ = rhs.as<Nodecl::Mul>();
//                 lhs_rhs = rhs_.get_lhs();
//                 rhs_rhs = rhs_.get_rhs();
//             }
//
//             Utils::InductionVarList loop_ivs = loop->get_induction_variables();
//             _constant = NBase::null();
//             if(Utils::induction_variable_list_contains_variable(loop_ivs, lhs_rhs))
//             {
//                 _constant = rhs_rhs;
//                 family = lhs_rhs;
//             }
//             else if(Utils::induction_variable_list_contains_variable(loop_ivs, rhs_rhs))
//             {
//                 _constant= lhs_rhs;
//                 family = rhs_rhs;
//             }
//             if(!_constant.is_null())
//             {
//                 if(is_loop_invariant(loop_entry, id_end))
//                 {   //! expression of type: "lhs = family (+,*) _constant"
//                     if(/*!loop_ivs.at(family).is_basic()*/ true)
//                     {
//                         // The only definition of \family that reaches \lhs is within the loop
//                         _constant = family;
//                         if(only_definition_is_in_loop(st, current, loop))
//                         // The family of \family must not be defined between the definition of \family and \lhs
//                         // TODO
//                         if(true)
//                         {
//                             res = lhs;
//                         }
//                     }
//                     else
//                     {
//                         res = lhs;
//                     }
//                 }
//             }
//         }
//
//         if(!check_potential_induction_variable(res, st, loop_entry, id_end))
//         {
//             res = NBase::null();
//         }

        return res;
    }

    bool InductionVariableAnalysis::check_potential_induction_variable(const NBase& iv, NBase& incr,
                                                                        NodeclList& incr_list,
                                                                        const NBase& stmt, Node* loop)
    {
        // Check whether the variable is modified in other places inside the loop
        Node* entry = loop->get_graph_entry_node();
        bool res = check_undesired_modifications(iv, incr, incr_list, stmt, entry, loop);
        ExtensibleGraph::clear_visits_aux(entry);

        // Check whether the variable is always the same memory location (avoid things like a[b[0]]++)
        res = !res && check_constant_memory_access(iv, loop);
        ExtensibleGraph::clear_visits_aux(loop);

        return res;
    }

    bool InductionVariableAnalysis::check_undesired_modifications(const NBase& iv, NBase& incr,
                                                                   NodeclList& incr_list,
                                                                   const NBase& stmt, Node* node, Node* loop)
    {
        bool result = false;

        if((node->get_id() != loop->get_graph_exit_node()->get_id()) && !node->is_visited_aux())
        {
            node->set_visited_aux(true);

            // Check the current node
            NodeclList stmts = node->get_statements();
            for(NodeclList::iterator it = stmts.begin(); it != stmts.end(); ++it)
            {
                // Check the statement only if it is not the statement where the potential IV was found
                if(!Nodecl::Utils::structurally_equal_nodecls(stmt, *it, /* skip conversion nodes */ true))
                {
                    FalseInductionVariablesVisitor v(iv, &incr, &incr_list, loop);
                    v.walk(*it);
                    if(!v.get_is_induction_variable())
                    {
                        result = true;
                        break;
                    }
                }
            }

            // If IV still looks like an IV, check for false positives in the children nodes
            if(!result)
            {
                ObjectList<Node*> children = node->get_children();
                for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    if(!check_undesired_modifications(iv, incr, incr_list, stmt, *it, loop))
                    {
                        result = true;
                        break;
                    }
                }
            }
        }

        return result;
    }

    bool InductionVariableAnalysis::check_constant_memory_access(const NBase& iv, Node* loop)
    {
        bool res = true;

        if(iv.is<Nodecl::Symbol>() || iv.is<Nodecl::ClassMemberAccess>())
        {}      // Nothing to be done: this will always be the same memory location
        else if(iv.is<Nodecl::ArraySubscript>())
        {
            Nodecl::ArraySubscript iv_as = iv.as<Nodecl::ArraySubscript>();
            Nodecl::List subscripts = iv_as.get_subscripts().as<Nodecl::List>();
            for(Nodecl::List::iterator it = subscripts.begin(); it != subscripts.end() && res; ++it)
            {
                if(!ExtensibleGraph::is_constant_in_context(loop, *it))
                    res = false;
            }
        }
        else if(iv.is<Nodecl::Dereference>())
        {
            WARNING_MESSAGE("Dereference as Induction Variables analysis is not yet supported\n", 0);
        }
        else
        {
            WARNING_MESSAGE("Unexpected type of node '%s' as Induction Variable\n", ast_print_node_type(iv.get_kind()));
        }

        return res;
    }

    Utils::InductionVarsPerNode InductionVariableAnalysis::get_all_induction_vars() const
    {
        return _induction_vars;
    }

    // ************************ END class for induction variables analysis ************************* //
    // ********************************************************************************************* //



    // ********************************************************************************************* //
    // ****************** Visitor that checks whether a potential IV is a real IV ****************** //

    FalseInductionVariablesVisitor::FalseInductionVariablesVisitor(NBase iv, NBase* incr,
                                                                    NodeclList* incr_list, Node* loop)
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

    void FalseInductionVariablesVisitor::visit(const Nodecl::AddAssignment& n)
    {
        if(Nodecl::Utils::dataref_contains_dataref(n.get_lhs(), _iv) ||
            Nodecl::Utils::dataref_contains_dataref(_iv, n.get_lhs()))
        {
            if(_n_nested_conditionals > 0)
            {
                // An induction variable may never be modified inside a conditional block
                undefine_induction_variable();
            }
            else
            {
                NBase new_incr;
                // Check whether the statement has the accepted syntax of an induction variable
                if(is_accepted_induction_variable_syntax(_loop, n, _iv, new_incr))
                {
                    // Check if the increments are linear and can be combined
                    if(const_value_is_positive(_incr->get_constant()) &&
                        const_value_is_positive(new_incr.get_constant()))
                    {
                        _incr_list->insert(new_incr);
                        NBase c = Nodecl::Add::make(*_incr, new_incr, _incr->get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        *_incr = const_value_to_nodecl(c_value);
                    }
                    else if(const_value_is_negative(_incr->get_constant()) &&
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
            }
        }
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::ArithmeticShrAssignment& n)
    {
        if(Nodecl::Utils::dataref_contains_dataref(n.get_lhs(), _iv) ||
            Nodecl::Utils::dataref_contains_dataref(_iv, n.get_lhs()))
        {
            undefine_induction_variable();
        }
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::Assignment& n)
    {
        if(Nodecl::Utils::dataref_contains_dataref(n.get_lhs(), _iv) ||
            Nodecl::Utils::dataref_contains_dataref(_iv, n.get_lhs()))
        {
            if(_n_nested_conditionals > 0)
            {
                // An induction variable may never be modified inside a conditional block
                undefine_induction_variable();
            }
            else
            {
                NBase new_incr;
                // Check whether the statement has the accepted syntax of an induction variable
                if(is_accepted_induction_variable_syntax(_loop, n, _iv, new_incr))
                {
                    // Check if the increments are linear and can be combined
                    if(const_value_is_positive(_incr->get_constant()) &&
                        const_value_is_positive(new_incr.get_constant()))
                    {
                        _incr_list->insert(new_incr);
                        NBase c = Nodecl::Add::make(*_incr, new_incr, _incr->get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        *_incr = const_value_to_nodecl(c_value);

                    }
                    else if(const_value_is_negative(_incr->get_constant()) &&
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
            }
        }
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::BitwiseAndAssignment& n)
    {
        if(Nodecl::Utils::dataref_contains_dataref(n.get_lhs(), _iv) ||
            Nodecl::Utils::dataref_contains_dataref(_iv, n.get_lhs()))
        {
            undefine_induction_variable();
        }
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::BitwiseOrAssignment& n)
    {
        if(Nodecl::Utils::dataref_contains_dataref(n.get_lhs(), _iv) ||
            Nodecl::Utils::dataref_contains_dataref(_iv, n.get_lhs()))
        {
            undefine_induction_variable();
        }
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::BitwiseShlAssignment& n)
    {
        if(Nodecl::Utils::dataref_contains_dataref(n.get_lhs(), _iv) ||
            Nodecl::Utils::dataref_contains_dataref(_iv, n.get_lhs()))
        {
            undefine_induction_variable();
        }
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::BitwiseShrAssignment& n)
    {
        if(Nodecl::Utils::dataref_contains_dataref(n.get_lhs(), _iv) ||
            Nodecl::Utils::dataref_contains_dataref(_iv, n.get_lhs()))
        {
            undefine_induction_variable();
        }
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::BitwiseXorAssignment& n)
    {
        if(Nodecl::Utils::dataref_contains_dataref(n.get_lhs(), _iv) ||
            Nodecl::Utils::dataref_contains_dataref(_iv, n.get_lhs()))
        {
            undefine_induction_variable();
        }
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
        if(Nodecl::Utils::dataref_contains_dataref(n.get_lhs(), _iv) ||
            Nodecl::Utils::dataref_contains_dataref(_iv, n.get_lhs()))
        {
            undefine_induction_variable();
        }
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
        if(Nodecl::Utils::dataref_contains_dataref(n.get_lhs(), _iv) ||
            Nodecl::Utils::dataref_contains_dataref(_iv, n.get_lhs()))
        {
            if(_n_nested_conditionals > 0)
            {
                // An induction variable may never be modified inside a conditional block
                undefine_induction_variable();
            }
            else
            {
                NBase new_incr;
                // Check whether the statement has the accepted syntax of an induction variable
                if(is_accepted_induction_variable_syntax(_loop, n, _iv, new_incr))
                {
                    // Check if the increments are linear and can be combined
                    if(const_value_is_positive(_incr->get_constant()) &&
                        const_value_is_positive(new_incr.get_constant()))
                    {
                        _incr_list->insert(new_incr);
                        NBase c = Nodecl::Add::make(*_incr, new_incr, _incr->get_type());
                        const_value_t* c_value = _calc.compute_const_value(c);
                        *_incr = const_value_to_nodecl(c_value);
                    }
                    else if(const_value_is_negative(_incr->get_constant()) &&
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
            }
        }
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::ModAssignment& n)
    {
        if(Nodecl::Utils::dataref_contains_dataref(n.get_lhs(), _iv) ||
            Nodecl::Utils::dataref_contains_dataref(_iv, n.get_lhs()))
        {
            undefine_induction_variable();
        }
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::MulAssignment& n)
    {
        if(Nodecl::Utils::dataref_contains_dataref(n.get_lhs(), _iv) ||
            Nodecl::Utils::dataref_contains_dataref(_iv, n.get_lhs()))
        {
            undefine_induction_variable();
        }
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::Postdecrement& n)
    {
        if(Nodecl::Utils::dataref_contains_dataref(n.get_rhs(), _iv) ||
            Nodecl::Utils::dataref_contains_dataref(_iv, n.get_rhs()))
        {
            if(_n_nested_conditionals > 0)
            {
                // An induction variable may never be modified inside a conditional block
                undefine_induction_variable();
            }
            else
            {
                // Check if the increments are linear and can be combined
                if(const_value_is_negative(_incr->get_constant()))
                {
                    NBase new_incr = const_value_to_nodecl(const_value_get_one(/* bytes */ 4, /* signed */ 1));
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
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::Postincrement& n)
    {
        if(Nodecl::Utils::dataref_contains_dataref(n.get_rhs(), _iv) ||
            Nodecl::Utils::dataref_contains_dataref(_iv, n.get_rhs()))
        {
            if(_n_nested_conditionals > 0)
            {
                // An induction variable may never be modified inside a conditional block
                undefine_induction_variable();
            }
            else
            {
                // Check if the increments are linear and can be combined
                if(const_value_is_positive(_incr->get_constant()))
                {
                    NBase new_incr = const_value_to_nodecl(const_value_get_one(/* bytes */ 4, /* signed */ 1));
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
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::Predecrement& n)
    {
        if(Nodecl::Utils::dataref_contains_dataref(n.get_rhs(), _iv) ||
            Nodecl::Utils::dataref_contains_dataref(_iv, n.get_rhs()))
        {
            if(_n_nested_conditionals > 0)
            {
                // An induction variable may never be modified inside a conditional block
                undefine_induction_variable();
            }
            else
            {
                // Check if the increments are linear and can be combined
                if(const_value_is_negative(_incr->get_constant()))
                {
                    NBase new_incr = const_value_to_nodecl(const_value_get_one(/* bytes */ 4, /* signed */ 1));
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
    }

    void FalseInductionVariablesVisitor::visit(const Nodecl::Preincrement& n)
    {
        if(Nodecl::Utils::dataref_contains_dataref(n.get_rhs(), _iv) ||
            Nodecl::Utils::dataref_contains_dataref(_iv, n.get_rhs()))
        {
            if(_n_nested_conditionals > 0)
            {
                // An induction variable may never be modified inside a conditional block
                undefine_induction_variable();
            }
            else
            {
                // Check if the increments are linear and can be combined
                if(const_value_is_positive(_incr->get_constant()))
                {
                    NBase new_incr = const_value_to_nodecl(const_value_get_one(/* bytes */ 4, /* signed */ 1));
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
    }

    // ************************ END class for induction variables analysis ************************* //
    // ********************************************************************************************* //
}
}
