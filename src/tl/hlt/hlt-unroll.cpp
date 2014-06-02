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




#include "hlt-unroll.hpp"
#include "tl-analysis-static-info.hpp"
#include <sstream>
#include <limits.h>

namespace TL { namespace HLT {

    LoopUnroll::LoopUnroll(Nodecl::NodeclBase for_stmt, unsigned int factor)
        : Transform(for_stmt), _tree(for_stmt), _factor(factor)
    {
    }

    bool LoopUnroll::check(bool diagnostic)
    {
        if (!_tree.is<Nodecl::ForStatement>())
        {
            if (diagnostic)
            {
                std::cerr << _tree.get_locus_str() << ": error: only for-statement can be unrolled" << std::endl;
            }
            return false;
        }

        // Now ask analysis to tell us the induction variables of this for statement
        // First get the enclosing function
        TL::Scope sc = ReferenceScope(_tree).get_scope();
        TL::Symbol function_symbol = sc.get_related_symbol();

        Nodecl::NodeclBase function_code = function_symbol.get_function_code();
        ERROR_CONDITION(function_code.is_null(), "Invalid node", 0);

        Analysis::AnalysisStaticInfo analysis_static(function_code, Analysis::WhichAnalysis::INDUCTION_VARS_ANALYSIS,
                                                     Analysis::WhereAnalysis::NESTED_FOR_STATIC_INFO, INT_MAX, /*ompss_enabled*/ false);

        // ObjectList<InductionVariableData*> induction_vars = analysis_static.get_induction_variables();

        return true;
    }
} }

