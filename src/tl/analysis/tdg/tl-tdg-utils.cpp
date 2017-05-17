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
#include "tl-task-dependency-graph.hpp"

namespace TL {
namespace Analysis {

    ReplaceAndEvalVisitor::ReplaceAndEvalVisitor(
            std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> lhs_vars,
            std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> rhs_vars)
        : _lhs_vars(lhs_vars), _rhs_vars(rhs_vars), _lhs(true)
    {}

    bool ReplaceAndEvalVisitor::unhandled_node(const NBase& n)
    {
        internal_error("Unhandled node of type '%s' while TDG ReplaceAndEval:\n '%s' ",
                        ast_print_node_type(n.get_kind()), n.prettyprint().c_str());
        return false;
    }

    bool ReplaceAndEvalVisitor::join_list(ObjectList<bool>& list)
    {
        for (ObjectList<bool>::iterator it = list.begin(); it != list.end(); ++it)
            if (*it == false)
                return false;
        return true;
    }

    bool ReplaceAndEvalVisitor::visit_comparison(const Nodecl::NodeclBase& n)
    {
        _lhs = true;
        walk(n.as<Nodecl::Equal>().get_lhs());
        _lhs = false;
        walk(n.as<Nodecl::Equal>().get_rhs());
        return false;
    }

    bool ReplaceAndEvalVisitor::visit(const Nodecl::Add& n)
    {
        NBase lhs = n.get_lhs();
        walk(lhs);
        ERROR_CONDITION(!lhs.is_constant(),
                        "Non constant node '%s' while ReplaceAndEval visit.\n",
                        lhs.prettyprint().c_str())
        NBase rhs = n.get_rhs();
        walk(rhs);
        ERROR_CONDITION(!rhs.is_constant(),
                        "Non constant node '%s' while ReplaceAndEval visit.\n",
                        rhs.prettyprint().c_str())
        const_value_t* sum = const_value_add(lhs.get_constant(), rhs.get_constant());
        ::nodecl_set_constant(n.get_internal_nodecl(), sum);
        return false;
    }

    bool ReplaceAndEvalVisitor::visit(const Nodecl::Conversion& n)
    {
        NBase nest = n.get_nest();
        walk(nest);
        ERROR_CONDITION(!nest.is_constant(),
                        "Non constant node '%s' while ReplaceAndEval visit.\n",
                        nest.prettyprint().c_str())
        ::nodecl_set_constant(n.get_internal_nodecl(), nest.get_constant());
        return false;
    }

    bool ReplaceAndEvalVisitor::visit(const Nodecl::Different& n)
    {
        visit_comparison(n);
        Utils::CmpResult cmp = Utils::compare_constants(
                n.get_lhs().get_constant(),
                n.get_rhs().get_constant());
        return cmp != Utils::CmpEqual;
    }

    bool ReplaceAndEvalVisitor::visit(const Nodecl::Equal& n)
    {
        visit_comparison(n);
        Utils::CmpResult cmp = Utils::compare_constants(
                n.get_lhs().get_constant(),
                n.get_rhs().get_constant());
        return cmp == Utils::CmpEqual;
    }

    bool ReplaceAndEvalVisitor::visit(const Nodecl::GreaterOrEqualThan& n)
    {
        visit_comparison(n);
        Utils::CmpResult cmp = Utils::compare_constants(
                n.get_lhs().get_constant(),
                n.get_rhs().get_constant());
        return cmp != Utils::CmpSmaller;
    }

    bool ReplaceAndEvalVisitor::visit(const Nodecl::GreaterThan& n)
    {
        visit_comparison(n);
        Utils::CmpResult cmp = Utils::compare_constants(
                n.get_lhs().get_constant(),
                n.get_rhs().get_constant());
        return cmp == Utils::CmpBigger;
    }

    bool ReplaceAndEvalVisitor::visit(const Nodecl::IntegerLiteral& n)
    {
        return false;
    }

    bool ReplaceAndEvalVisitor::visit(const Nodecl::LogicalAnd& n)
    {
        return walk(n.get_lhs()) && walk(n.get_rhs());
    }

    bool ReplaceAndEvalVisitor::visit(const Nodecl::LogicalOr& n)
    {
        return walk(n.get_lhs()) || walk(n.get_rhs());
    }

    bool ReplaceAndEvalVisitor::visit(const Nodecl::LowerThan& n)
    {
        visit_comparison(n);
        Utils::CmpResult cmp = Utils::compare_constants(
                n.get_lhs().get_constant(),
                n.get_rhs().get_constant());
        return cmp == Utils::CmpSmaller;
    }

    bool ReplaceAndEvalVisitor::visit(const Nodecl::LowerOrEqualThan& n)
    {
        visit_comparison(n);
        Utils::CmpResult cmp = Utils::compare_constants(
                n.get_lhs().get_constant(),
                n.get_rhs().get_constant());
        return cmp != Utils::CmpBigger;
    }

    bool ReplaceAndEvalVisitor::visit(const Nodecl::Minus& n)
    {
        NBase lhs = n.get_lhs();
        walk(lhs);
        ERROR_CONDITION(!lhs.is_constant(),
                        "Non constant node '%s' while ReplaceAndEval visit.\n",
                        lhs.prettyprint().c_str())
        NBase rhs = n.get_rhs();
        walk(rhs);
        ERROR_CONDITION(!rhs.is_constant(),
                        "Non constant node '%s' while ReplaceAndEval visit.\n",
                        rhs.prettyprint().c_str())
        const_value_t* sum = const_value_sub(lhs.get_constant(), rhs.get_constant());
        ::nodecl_set_constant(n.get_internal_nodecl(), sum);
        return false;
    }

    bool ReplaceAndEvalVisitor::visit(const Nodecl::Symbol& n)
    {
        if (_lhs)
        {
            std::map<NBase, const_value_t*>::iterator it = _lhs_vars.find(n);
            ERROR_CONDITION(it == _lhs_vars.end(),
                            "No symbol '%s' found in the map of variables of the LHS.\n",
                            n.get_symbol().get_name().c_str());
            n.replace(NBase(const_value_to_nodecl(it->second)));
        }
        else
        {
            std::map<NBase, const_value_t*>::iterator it = _rhs_vars.find(n);
            ERROR_CONDITION(it == _rhs_vars.end(),
                            "No symbol '%s' found in the map of variables of the RHS.\n",
                            n.get_symbol().get_name().c_str());
            n.replace(NBase(const_value_to_nodecl(it->second)));
        }

        return true;
    }

}
}