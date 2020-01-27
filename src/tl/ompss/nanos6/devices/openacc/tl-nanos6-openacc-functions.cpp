/*--------------------------------------------------------------------
  (C) Copyright 2020-2020 Barcelona Supercomputing Center
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

#include "tl-nanos6-openacc-functions.hpp"

namespace TL
{
namespace Nanos6
{

OpenACCTasks::OpenACCTasks()
{
    set_phase_name("Nanos6/OpenACC specific pass");
    set_phase_description(
        "This pass makes sure all the device specific information that Nanos6 "
        "has is properly passed to OpenACC directives.");
}

void OpenACCTasks::run(DTO &dto)
{
    std::cout << "Running pass!\n";
}

} // namespace Nanos6
} // namespace TL

EXPORT_PHASE(TL::Nanos6::OpenACCTasks)
