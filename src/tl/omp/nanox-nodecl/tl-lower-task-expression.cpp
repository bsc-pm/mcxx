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

#include "tl-lowering-visitor.hpp"
#include "tl-lower-task-common.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-nanos.hpp"

namespace TL { namespace Nanox {

void LoweringVisitor::visit(const Nodecl::OpenMP::TaskExpression& task_expr)
{
    Nodecl::OpenMP::Task join_task = task_expr.get_join_task().as<Nodecl::OpenMP::Task>();
    Nodecl::List task_calls = task_expr.get_task_calls().as<Nodecl::List>();

    if (Nanos::Version::interface_is_at_least("master", 5024))
    {
        ERROR_CONDITION(!task_expr.get_parent().is<Nodecl::ExpressionStatement>(), "Unexpected node", 0);
        Nodecl::NodeclBase expr_stmt = task_expr.get_parent();

        TL::ObjectList<Nodecl::NodeclBase> items;
        Nodecl::NodeclBase is_in_final_nodecl;
        get_nanos_in_final_condition(task_expr, task_expr.get_locus(), is_in_final_nodecl, items);
        Nodecl::Utils::prepend_items_before(expr_stmt, Nodecl::List::make(items));

        Nodecl::List statements_task_expr;
         for (Nodecl::List::iterator it = task_calls.begin();
                 it != task_calls.end();
                 ++it)
         {
             Nodecl::ExpressionStatement current_expr_stmt = it->as<Nodecl::ExpressionStatement>();
             Nodecl::OpenMP::TaskCall current_task_call = current_expr_stmt.get_nest().as<Nodecl::OpenMP::TaskCall>();
             statements_task_expr.append(Nodecl::ExpressionStatement::make(current_task_call.get_call().shallow_copy()));
         }
         statements_task_expr.append(join_task.get_statements().shallow_copy());

        Nodecl::NodeclBase if_else_stmt =
            Nodecl::IfElseStatement::make(
                    is_in_final_nodecl,
                    Nodecl::List::make(
                        Nodecl::CompoundStatement::make(
                            statements_task_expr,
                            nodecl_null(),
                            task_expr.get_locus())),
                    Nodecl::List::make(
                        Nodecl::CompoundStatement::make(
                            Nodecl::List::make(Nodecl::ExpressionStatement::make(task_expr)),
                            nodecl_null(),
                            task_expr.get_locus())),
                    task_expr.get_locus());

        expr_stmt.replace(if_else_stmt);
    }

    // Note: don't walk over the OpenMP::Task node because Its visitor ignores
    // the placeholder and sets to false the 'inside_task_expression' boolean
    Nodecl::NodeclBase placeholder_task_expr_transformation;
    visit_task(join_task, /* inside_task_expression */ true, &placeholder_task_expr_transformation);

    // Note: don't walk over the OpenMP::TaskCall because It's visitor sets to
    // false the 'inside_task_expression' boolean
    for (Nodecl::List::iterator it = task_calls.begin();
            it != task_calls.end();
            ++it)
    {
        Nodecl::ExpressionStatement current_expr_stmt = it->as<Nodecl::ExpressionStatement>();
        Nodecl::OpenMP::TaskCall current_task_call = current_expr_stmt.get_nest().as<Nodecl::OpenMP::TaskCall>();
        visit_task_call(current_task_call, /* inside_task_expression */ true);

        Nodecl::Utils::prepend_items_before(placeholder_task_expr_transformation, *it);
    }

    ERROR_CONDITION(!task_expr.get_parent().is<Nodecl::ExpressionStatement>(), "Unexpected node", 0);
    Nodecl::NodeclBase expr_stmt = task_expr.get_parent();
    Nodecl::Utils::prepend_items_before(expr_stmt, task_expr.get_join_task());

    // Finally, remove from the tree the TaskExpression node
    Nodecl::Utils::remove_from_enclosing_list(expr_stmt);
}

}}
