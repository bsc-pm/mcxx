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

#include "hlt-utils.hpp"

namespace TL {namespace HLT { namespace Utils {

Nodecl::NodeclBase compute_induction_variable_final_expr(
        const Nodecl::NodeclBase& loop)
{
    ERROR_CONDITION(!loop.is<Nodecl::ForStatement>(), "Invalid tree", 0);

    TL::ForStatement for_stmt(loop.as<Nodecl::ForStatement>());

    ERROR_CONDITION(!for_stmt.is_omp_valid_loop(), "Invalid loop", 0);

    Nodecl::NodeclBase lower_bound = for_stmt.get_lower_bound().shallow_copy();
    Nodecl::NodeclBase upper_bound = for_stmt.get_upper_bound().shallow_copy();
    Nodecl::NodeclBase step = for_stmt.get_step().shallow_copy();

    // This expression computes the ceiling division between the iteration space
    // and the step, to get the effective number of elements for this loop.
    //
    //     - Ceiling division A/B can be computed as (A + B +/- 1)/B
    //       (+/- depending on the sign of A and B)
    //
    //     - TL::ForStatement utility normalizes the upper bound (UB) so that
    //       this +/- 1 is already computed in its value, therefore, the ceiling
    //       division is computed as (UB - LB + step) / step
    Nodecl::NodeclBase num_elements =
        Nodecl::Div::make(
                Nodecl::ParenthesizedExpression::make(
                    Nodecl::Add::make(
                        Nodecl::Minus::make(
                            Nodecl::ParenthesizedExpression::make(
                                upper_bound,
                                upper_bound.get_type()),
                            Nodecl::ParenthesizedExpression::make(
                                lower_bound,
                                lower_bound.get_type()),
                            lower_bound.get_type()),
                        step,
                        step.get_type()),
                    step.get_type()),
                step.shallow_copy(),
                step.get_type());

    Nodecl::NodeclBase expr =
        Nodecl::Add::make(
                lower_bound.shallow_copy(),
                Nodecl::ParenthesizedExpression::make(
                    Nodecl::Mul::make(
                        num_elements,
                        step.shallow_copy(),
                        num_elements.get_type()),
                    num_elements.get_type()),
                num_elements.get_type());

    return expr;
}

} } }
