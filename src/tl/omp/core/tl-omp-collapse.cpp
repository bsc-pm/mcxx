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




#include "tl-omp-core.hpp"
#include "cxx-diagnostic.h"
#include "cxx-cexpr.h"
// #include "hlt-collapse.hpp"
// #include "tl-ast.hpp"

namespace TL
{
    namespace OpenMP
    {
        void Core::collapse_check_loop(TL::PragmaCustomStatement construct)
        {
            TL::PragmaCustomClause collapse = construct.get_pragma_line().get_clause("collapse");
            if (!collapse.is_defined())
                return;

            TL::ObjectList<Nodecl::NodeclBase> expr_list = collapse.get_arguments_as_expressions(construct);

            if (expr_list.size() != 1)
            {
                error_printf_at(construct.get_locus(), "collapse clause needs exactly one argument\n");
                return;
            }

            Nodecl::NodeclBase expr = expr_list[0];
            if (!expr.is_constant()
                    || !is_any_int_type(expr.get_type().get_internal_type()))
            {
                error_printf_at(construct.get_locus(),
                        "collapse clause requires an integer constant expression\n");
                return;
            }

            const_value_t* cval = expr.get_constant();

            if (!const_value_is_one(cval))
            {
                error_printf_at(construct.get_locus(),
                        "only collapse(1) is supported\n");
                return;
            }
        }
    }
}
