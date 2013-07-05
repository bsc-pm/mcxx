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

    if (!_lowering->final_clause_transformation_disabled()
            && Nanos::Version::interface_is_at_least("master", 5024))
    {
        ERROR_CONDITION(!task_expr.get_parent().is<Nodecl::ExpressionStatement>(), "Unexpected node", 0);
        Nodecl::NodeclBase expr_stmt = task_expr.get_parent();

        TL::Source code;
        code
            << "{"
            <<      as_type(TL::Type::get_bool_type()) << "mcc_is_in_final;"
            <<      "nanos_err_t mcc_err_in_final = nanos_in_final(&mcc_is_in_final);"
            <<      "if (mcc_is_in_final)"
            <<      "{"
            <<          as_statement(task_expr.get_sequential_code())
            <<      "}"
            <<      "else"
            <<      "{"
            <<          as_statement(Nodecl::ExpressionStatement::make(task_expr))
            <<      "}"
            << "}"
            ;

        if (IS_FORTRAN_LANGUAGE)
            Source::source_language = SourceLanguage::C;

        Nodecl::NodeclBase if_else_tree = code.parse_statement(expr_stmt);

        if (IS_FORTRAN_LANGUAGE)
            Source::source_language = SourceLanguage::Current;

        expr_stmt.replace(if_else_tree);
    }

    // Note: don't walk over the OpenMP::Task node because Its visitor ignores
    // the placeholder and sets to false the 'inside_task_expression' boolean
    Nodecl::NodeclBase placeholder_task_expr_transformation;
    visit_task(task_expr.get_join_task().as<Nodecl::OpenMP::Task>(), /* inside_task_expression */ true, &placeholder_task_expr_transformation);

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
