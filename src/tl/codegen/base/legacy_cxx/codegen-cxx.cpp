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

#include "codegen-cxx.hpp"
#include "cxx-legacy-codegen.h"
#include <cstdio>

// This is a legacy codegen phase. It is here just for A/B testing with the existing code

namespace Codegen {

    std::string CxxLegacy::codegen(const Nodecl::NodeclBase& n)
    {
        if (this->is_file_output())
        {
            char *str = NULL;
            size_t size = 0;
            FILE* temporal_stream = ::open_memstream(&str, &size);

            _c_cxx_codegen_translation_unit(temporal_stream, n.get_internal_nodecl());

            fclose(temporal_stream);

            std::string result = str;
            free(str);
            return result;
        }
        else
        {
            return _c_cxx_codegen_to_str(n.get_internal_nodecl());
        }
    }
}

EXPORT_PHASE(Codegen::CxxLegacy)
