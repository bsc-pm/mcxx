/*--------------------------------------------------------------------
  (C) Copyright 2015-2015 Barcelona Supercomputing Center
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

#include "tl-nanos6-device.hpp"

#include "tl-nodecl-utils.hpp"

namespace TL { namespace Nanos6 {

    Nodecl::NodeclBase Device::compute_specific_task_body(
            Nodecl::NodeclBase task_body,
            const DirectiveEnvironment &env,
            Nodecl::NodeclBase unpacked_function_code,
            const TL::Scope &unpacked_inside_scope,
            Nodecl::Utils::SimpleSymbolMap &symbol_map)
    {
        return Nodecl::Utils::deep_copy(task_body, unpacked_inside_scope, symbol_map);
    }

void Device::root_unpacked_function(TL::Symbol unpacked_function, Nodecl::NodeclBase unpacked_function_code)
{
    Nodecl::Utils::append_to_top_level_nodecl(unpacked_function_code);
}

} }
