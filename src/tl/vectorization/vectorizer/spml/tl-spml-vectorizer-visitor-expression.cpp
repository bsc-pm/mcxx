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

#include "tl-spml-vectorizer-visitor-expression.hpp"

#include "tl-vectorization-utils.hpp"
#include "tl-vectorization-analysis-interface.hpp"

#include "cxx-cexpr.h"
#include "tl-nodecl-utils.hpp"


namespace TL
{
namespace Vectorization
{
    SPMLVectorizerVisitorExpression::SPMLVectorizerVisitorExpression(
            VectorizerEnvironment& environment)
        : VectorizerVisitorExpression(environment)
    {
    }

    void SPMLVectorizerVisitorExpression::visit(const Nodecl::FunctionCall& n)
    {
        Nodecl::NodeclBase mask = Utils::get_proper_mask(
                _environment._mask_list.back());

        Nodecl::NodeclBase called = n.get_called();
        ERROR_CONDITION(!called.is<Nodecl::Symbol>(),
                "Vectorizer: %s found. This kind of function call is not "\
                "supported yet", ast_print_node_type(called.get_kind()));

        Nodecl::Symbol called_sym = called.as<Nodecl::Symbol>();
        TL::Type call_type = n.get_type();
        std::string func_name = called_sym.get_symbol().get_name();

        if (func_name == "omp_get_num_threads")
        {
            const Nodecl::Mul spml_num_lanes =
                Nodecl::Mul::make(
                        n.shallow_copy(),
                        const_value_to_nodecl(const_value_get_signed_int(
                                _environment._vectorization_factor)),
                        n.get_type(),
                        n.get_locus());

            n.replace(spml_num_lanes);
        }
        else
        {
            VectorizerVisitorExpression::visit(n);
        }

    }
}
}
