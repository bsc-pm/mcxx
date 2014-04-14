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

#ifndef TL_VECTORIZATION_ANALYSIS_HPP
#define TL_VECTORIZATION_ANALYSIS_HPP

#include "tl-analysis-interface.hpp"
//#include "tl-analysis-static-info.hpp"
//#include "tl-analysis-singleton.hpp"

//#include "tl-induction-variables-data.hpp"
//#include "tl-nodecl-visitor.hpp"
//#include "tl-objectlist.hpp"
//#include "tl-omp.hpp"

namespace TL {
namespace Analysis {

    class VectorizationAnalysis : public AnalysisInterface
    {
        public:
            VectorizationAnalysis( const Nodecl::NodeclBase& n, WhichAnalysis analysis_mask,
                                WhereAnalysis nested_analysis_mask, int nesting_level );

            ~VectorizationAnalysis();

            virtual bool is_adjacent_access(const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n);
    };
}
}

#endif // TL_VECTORIZATION_ANALYSIS_HPP
