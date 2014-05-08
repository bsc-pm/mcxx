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

#include "tl-analysis-singleton.hpp"
#include "tl-complexity.hpp"

namespace TL {

    // ********************************************************************************************* //
    // ********************************* Complexity Analysis phase ********************************* //
    
    Complexity::Complexity()
        : _disable_phase("0")
    {
        set_phase_name("Cyclomatic Complexity Analysis");
        set_phase_description("This phase computes the cyclomatic complexity of each function in the input code");

        register_parameter("disable-complexity",
                "Disables this phase. You should not need this. If you do, then it is an error. Please fill a bug",
                _disable_phase,
                "0");
    }

    void Complexity::run(TL::DTO& dto)
    {
        Nodecl::NodeclBase top_level = dto["nodecl"];

        TL::Analysis::AnalysisSingleton& singleton = TL::Analysis::AnalysisSingleton::get_analysis();
        TL::Analysis::PCFGAnalysis_memento memento;
        singleton.cyclomatic_complexity(memento, top_level);
    }

    void Complexity::pre_run(TL::DTO& dto)
    {}
    
    // ******************************* END Complexity Analysis phase ******************************* //
    // ********************************************************************************************* //
}

EXPORT_PHASE(TL::Complexity);