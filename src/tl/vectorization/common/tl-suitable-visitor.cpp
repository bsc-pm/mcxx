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

#include "tl-suitable-visitor.hpp"
#include "tl-vectorizer.hpp"
#include "cxx-cexpr.h"

namespace TL
{
namespace Vectorization
{
SuitableVisitor::SuitableVisitor(const Nodecl::NodeclBase &scope,
                                 const objlist_nodecl_t &suitable_expressions,
                                 const unsigned int suitable_factor,
                                 const unsigned int vec_factor,
                                 VectorizationAnalysisInterface *analysis)
    : _scope(scope),
      _suitable_expressions(suitable_expressions),
      _analysis(analysis),
      _suitable_factor(suitable_factor),
      _vec_factor(vec_factor)
{
}

int SuitableVisitor::join_list(ObjectList<int> &list)
{
    int result = 0;
    for (const auto &it : list)
    {
        result = result + it;
    }
    return result;
}

bool SuitableVisitor::is_suitable_expression(Nodecl::NodeclBase n)
{
    bool result = false;
    if (Nodecl::Utils::list_contains_nodecl_by_structure(_suitable_expressions,
                                                         n))
        result = true;

    if (!result && n.is<Nodecl::Symbol>())
    {
        TL::Symbol tl_sym = n.as<Nodecl::Symbol>().get_symbol();
        // VLA dimension
        if (tl_sym.is_saved_expression())
        {
            if (Nodecl::Utils::list_contains_nodecl_by_structure(
                    _suitable_expressions, tl_sym.get_value().no_conv()))
                result = true;
        }
    }

    return result;
}

bool SuitableVisitor::is_suitable_constant(int n)
{
    if ((n % _suitable_factor) == 0)
        return true;
    else
        return false;
}

int SuitableVisitor::visit(const Nodecl::Add &n)
{
    if (is_suitable_expression(n))
    {
        return _suitable_factor;
    }

    int lhs_mod = walk(n.get_lhs());
    int rhs_mod = walk(n.get_rhs());

    if ((lhs_mod != -1) && (rhs_mod != -1))
        return lhs_mod + rhs_mod;

    return -1;
}

int SuitableVisitor::visit(const Nodecl::ArraySubscript &n)
{
    if (is_suitable_expression(n))
    {
        return _suitable_factor;
    }

    return -1;
}

int SuitableVisitor::visit(const Nodecl::BitwiseShl &n)
{
    if (is_suitable_expression(n))
    {
        return _suitable_factor;
    }

    int lhs_mod = walk(n.get_lhs());
    int rhs_mod = walk(n.get_rhs());

    // Something suitable multiplied by anything is suitable
    if (rhs_mod > 0)
    {
        // Because a << const is: a * (1 << const)
        if ((is_suitable_constant(lhs_mod))
            || (is_suitable_constant(1 << rhs_mod)))
            return 0;
        else if ((lhs_mod != -1) && (rhs_mod != -1))
            return lhs_mod << rhs_mod;
    }

    return -1;
}

int SuitableVisitor::visit(const Nodecl::BitwiseShr &n)
{
    if (is_suitable_expression(n))
    {
        return _suitable_factor;
    }

    int lhs_mod = walk(n.get_lhs());
    int rhs_mod = walk(n.get_rhs());

    // Something suitable multiplied by anything is suitable
    if (rhs_mod > 0)
    {
        // Because a << const is: a / (1 << const)
        if ((is_suitable_constant(lhs_mod))
            || (is_suitable_constant(1 << rhs_mod)))
            return 0;
        else if ((lhs_mod > 0) && (rhs_mod > 0))
            return lhs_mod >> rhs_mod;
    }

    return -1;
}

int SuitableVisitor::visit(const Nodecl::Conversion &n)
{
    if (is_suitable_expression(n))
    {
        return _suitable_factor;
    }

    return walk(n.get_nest());
}

int SuitableVisitor::visit(const Nodecl::IntegerLiteral &n)
{
    return const_value_cast_to_signed_int(n.get_constant());
}

int SuitableVisitor::visit(const Nodecl::Neg &n)
{
    if (is_suitable_expression(n))
    {
        return _suitable_factor;
    }

    int rhs_mod = walk(n.get_rhs());

    if (rhs_mod != -1)
    {
        int result = -rhs_mod;
        if (result < 0)
            result = _suitable_factor + result;

        return result;
    }

    return -1;
}

int SuitableVisitor::visit(const Nodecl::Minus &n)
{
    if (is_suitable_expression(n))
    {
        return _suitable_factor;
    }

    int lhs_mod = walk(n.get_lhs());
    int rhs_mod = walk(n.get_rhs());

    if ((lhs_mod != -1) && (rhs_mod != -1))
    {
        return lhs_mod - rhs_mod;
    }

    return -1;
}

int SuitableVisitor::visit(const Nodecl::Mul &n)
{
    if (is_suitable_expression(n))
    {
        return _suitable_factor;
    }

    int lhs_mod = walk(n.get_lhs());
    int rhs_mod = walk(n.get_rhs());

    // Something suitable multiplied by anything is suitable
    if ((is_suitable_constant(lhs_mod)) || (is_suitable_constant(rhs_mod)))
        return _suitable_factor;
    else if ((lhs_mod != -1) && (rhs_mod != -1))
        return lhs_mod * rhs_mod;

    return -1;
}

int SuitableVisitor::visit(const Nodecl::Symbol &n)
{
    if (is_suitable_expression(n))
    {
        return _suitable_factor;
    }
    else if (n.is_constant())
    {
        return const_value_cast_to_signed_int(n.get_constant());
    }
    // IV of the SIMD loop
    else if (_analysis->is_linear(_scope, n))
    {
        Nodecl::NodeclBase lb
            = _analysis->get_induction_variable_lower_bound(_scope, n);

        // TL::Analysis::NodeclSet lb_set =
        //    _analysis->get_linear_variable_lower_bound(_scope, n);
        //
        //// TODO: Multiple lb
        // if (lb_set.size() > 1)
        //    abort();
        // Nodecl::NodeclBase lb = *lb_set.begin();

        Nodecl::NodeclBase incr = _analysis->get_linear_step(_scope, n);

        int lb_mod = walk(lb);
        int incr_mod = walk(incr);

        if (lb_mod != -1 && incr_mod != -1)
            return lb_mod + incr_mod * _vec_factor;
    }
    else // Try to get information of the evolution of 'n'
    {
        Nodecl::ForStatement enclosing_for_stmt
            = Nodecl::Utils::get_enclosing_nodecl_of_kind<Nodecl::ForStatement>(
                  n)
                  .as<Nodecl::ForStatement>();

        while (!enclosing_for_stmt.is_null())
        {
            if (_analysis->is_linear(enclosing_for_stmt, n))
            {
                Nodecl::NodeclBase lb
                    = _analysis->get_induction_variable_lower_bound(
                        enclosing_for_stmt, n);
                Nodecl::NodeclBase incr
                    = _analysis->get_linear_step(enclosing_for_stmt, n);

                // for(j=j;
                if (!(Nodecl::Utils::structurally_equal_nodecls(lb, n, true)
                      || Nodecl::Utils::structurally_equal_nodecls(
                             incr, n, true)))
                {
                    int lb_mod = walk(lb);
                    int incr_mod = walk(incr);

                    if (lb_mod != -1 && incr_mod != -1
                        && ((lb_mod % _vec_factor) == 0)
                        && ((incr_mod % _vec_factor) == 0))
                    {
                        return lb_mod + incr_mod;
                    }

                    break;
                }
            }

            enclosing_for_stmt
                = Nodecl::Utils::
                      get_enclosing_nodecl_of_kind<Nodecl::ForStatement>(
                          enclosing_for_stmt.get_parent())
                          .as<Nodecl::ForStatement>();
        }
    }

    return -1;
}

int SuitableVisitor::unhandled_node(const Nodecl::NodeclBase &n)
{
    WARNING_MESSAGE("Suitable Alignment Visitor: Unknown node '%s' at '%s'\n",
                    ast_print_node_type(n.get_kind()),
                    n.get_locus_str().c_str());
    return -1;
}
}
}
