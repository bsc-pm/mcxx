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

#ifndef TL_PARALLEL_COMMON_HPP
#define TL_PARALLEL_COMMON_HPP

#include "tl-omp-nanox.hpp"
#include "tl-data-env.hpp"

namespace TL
{
    namespace Nanox
    {
        Source common_parallel_spawn_code(Source num_devices,
                Source outline_name,
                Source struct_arg_type_name,
                Source num_threads,
                const DataEnvironInfo& data_environ_info);
    }
}

#endif // TL_PARALLEL_COMMON_HPP
