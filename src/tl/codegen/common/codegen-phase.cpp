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

#include "codegen-phase.hpp"
#include "tl-builtin.hpp"

namespace Codegen
{
    void CodegenPhase::run(TL::DTO& dto)
    {
        TL::File output_file = *std::static_pointer_cast<TL::File>(dto["output_file"]);
        FILE* f = output_file.get_file();

        TL::String output_filename_ = *std::static_pointer_cast<TL::String>(dto["output_filename"]);

        Nodecl::NodeclBase n = *std::static_pointer_cast<Nodecl::NodeclBase>(dto["nodecl"]);

        this->codegen_top_level(n, f, output_filename_);
    }
    void CodegenPhase::handle_parameter(int n, void* data)
    {}
}

Codegen::CodegenPhase& Codegen::get_current()
{
    CodegenPhase* result = reinterpret_cast<CodegenPhase*>(CURRENT_CONFIGURATION->codegen_phase);

    return *result;
}
