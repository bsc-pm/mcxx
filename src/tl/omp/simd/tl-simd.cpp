/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#include "tl-simd.hpp"
#include "tl-simd-ir-visitor.hpp"

namespace TL { 
    
    namespace SIMD {

        SimdIR::SimdIR()
        {
            set_phase_name("SIMD Intermediate Representation");
            set_phase_description("This phase introduces a generic SIMD representation into the Mercurium parallel IR");
        }

        void SimdIR::pre_run(DTO& dto)
        {
        }

        void SimdIR::run(DTO& dto)
        {
            std::cerr << "SIMD Intermediate Representation phase" << std::endl;

            Nodecl::NodeclBase n = dto["nodecl"];

            SimdIRVisitor simd_ir_visitor;
            simd_ir_visitor.walk(n);
        }
    } 
}

EXPORT_PHASE(TL::SIMD::SimdIR);
