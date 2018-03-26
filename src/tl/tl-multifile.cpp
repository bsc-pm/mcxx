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




#include "cxx-driver.h"
#include "tl-multifile.hpp"

namespace TL
{
    ObjectList<IncludeLine> CurrentFile::get_included_files()
    {
        ObjectList<IncludeLine> result;
        for (int i = 0; i < CURRENT_COMPILED_FILE->num_includes; i++)
        {
            include_t *include = CURRENT_COMPILED_FILE->include_list[i];

            IncludeLine include_line(include->included_file, include->system_include);
            result.push_back(include_line);
        }

        return result;
    }

    std::string IncludeLine::get_preprocessor_line()
    {
        if (is_system())
        {
            return std::string("#include <") + _file + std::string(">");
        }
        else
        {
            return std::string("#include \"") + _file + std::string("\"");
        }
    }
}
