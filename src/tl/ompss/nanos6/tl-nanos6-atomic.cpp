/*--------------------------------------------------------------------
  (C) Copyright 2015-2015 Barcelona Supercomputing Center
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


#include "tl-nanos6-lower.hpp"
#include "tl-omp-lowering-atomics.hpp"
#include "cxx-diagnostic.h"

namespace TL { namespace Nanos6 {

namespace
{
    Nodecl::NodeclBase make_mutual_exclusive_expr(Nodecl::ExpressionStatement expr_stmt)
    {
        Nodecl::NodeclBase expr =
            expr_stmt.as<Nodecl::ExpressionStatement>().get_nest();

        bool builtin_atomic = false;
        bool nanox_api_atomic = false; // Feasible atomic transformation using Nanox API

        if (!TL::OpenMP::Lowering::allowed_expression_atomic(expr, builtin_atomic, nanox_api_atomic)
                || nanox_api_atomic)
        {
            warn_printf_at(expr.get_locus(),
                    "'atomic' expression cannot be implemented efficiently, a critical region will be used instead\n");

            return Nodecl::OpenMP::Critical::make(
                    /* environment */ Nodecl::NodeclBase::null(),
                    Nodecl::List::make(
                        Nodecl::ExpressionStatement::make(expr)));
        }

        if (builtin_atomic)
            return TL::OpenMP::Lowering::builtin_atomic_int_op(expr);

        return TL::OpenMP::Lowering::compare_and_exchange(expr);
    }
}

    void Lower::visit(const Nodecl::OpenMP::Atomic& node)
    {
        Nodecl::NodeclBase context = node.get_statements()
            .as<Nodecl::List>().front();

        ERROR_CONDITION(!context.is<Nodecl::Context>(),
                "Unexpected node", 0);

        Nodecl::NodeclBase expr_statement = context
            .as<Nodecl::Context>().get_in_context()
            .as<Nodecl::List>().front();

        ERROR_CONDITION(!expr_statement.is<Nodecl::ExpressionStatement>(),
                "Unexpected node", 0);

        node.replace(make_mutual_exclusive_expr(
                    expr_statement.as<Nodecl::ExpressionStatement>()));

        walk(node);
    }

} }
