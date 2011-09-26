/*--------------------------------------------------------------------
(C) Copyright 2006-2009 Barcelona Supercomputing Center 
Centro Nacional de Supercomputacion

This file is part of Mercurium C/C++ source-to-source compiler.

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

#include "cxx-utils.h"
#include "tl-analysis-phase.hpp"
#include "tl-cfg-visitor.hpp"


namespace TL
{
    AnalysisPhase::AnalysisPhase()
    {
        set_phase_name("Experimental phase for analysis");
        set_phase_description("This phase builds a Control Flow Graph and performs different analysis on demand. ");
    }
    
    void AnalysisPhase::run(TL::DTO& dto)
    {
        RefPtr<Nodecl::NodeclBase> nodecl = RefPtr<Nodecl::NodeclBase>::cast_dynamic(dto["nodecl"]);
        
        CfgVisitor cfg_visitor;
        cfg_visitor.build_cfg(nodecl, std::string(""));
        
        // TODO with the list of functions with its correspondent list of graphs in @cfgs
        // now we can perform some kind of inter-procedural analysis
        ObjectList<ExtensibleGraph*> cfgs = cfg_visitor.get_cfgs();
//         ObjectList<ExtensibleGraph*> ipa_cfgs;
//         for (ObjectList<ExtensibleGraph*>::iterator it = cfgs.begin();
//             it != cfgs.end(); 
//             ++it)
//         {
//             
//         }
//         
//         // Perform Live Variable Analysis and Print the Graph to a dot file        
        for (ObjectList<ExtensibleGraph*>::iterator it = cfgs.begin();
            it != cfgs.end(); 
            ++it)
        {
            (*it)->live_variable_analysis();
            (*it)->print_graph_to_dot();
        }
    }
}

EXPORT_PHASE(TL::AnalysisPhase);
