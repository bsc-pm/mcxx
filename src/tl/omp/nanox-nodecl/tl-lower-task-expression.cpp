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

namespace TL { namespace Nanox {

void LoweringVisitor::visit(const Nodecl::OpenMP::TaskExpression& task_expr)
{
    Nodecl::OpenMP::Task join_task = task_expr.get_join_task().as<Nodecl::OpenMP::Task>();

    Nodecl::NodeclBase placeholder_task_expr_transformation;

    // Note: don't walk over the OpenMP::Task node because Its visitor ignores the placeholder
    visit_task(join_task, &placeholder_task_expr_transformation);

    walk(task_expr.get_task_calls());

    Nodecl::List task_calls = task_expr.get_task_calls().as<Nodecl::List>();

    ERROR_CONDITION(!task_expr.get_parent().is<Nodecl::ExpressionStatement>(), "Unexpected node", 0);
    Nodecl::NodeclBase expr_stmt = task_expr.get_parent();

    for (Nodecl::List::iterator it = task_calls.begin();
            it != task_calls.end();
            ++it)
    {
        Nodecl::Utils::prepend_items_before(placeholder_task_expr_transformation, *it);
    }

    Nodecl::Utils::prepend_items_before(expr_stmt, task_expr.get_join_task());

    // Finally, remove from the tree the TaskExpression node
    Nodecl::Utils::remove_from_enclosing_list(expr_stmt);
}

}}
