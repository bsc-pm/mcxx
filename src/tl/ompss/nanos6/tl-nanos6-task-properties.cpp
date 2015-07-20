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


#include "tl-nanos6-task-properties.hpp"


namespace TL { namespace Nanos6 {

    TaskProperties TaskProperties::gather_task_properties(const Nodecl::OpenMP::Task& node)
    {
        TaskProperties tp;

        return tp;
    }

    void TaskProperties::create_task_info(
            /* out */
            TL::Symbol &task_info,
            Nodecl::NodeclBase& local_init)
    {
    }

    void TaskProperties::create_info_structure(
            /* out */
            TL::Type& data_env_struct,
            Nodecl::NodeclBase& args_size)
    {
    }

    void TaskProperties::create_outline_function(
            /* out */
            TL::Symbol& outline_function)
    {
    }
    void TaskProperties::create_dependences_function(
            /* out */
            TL::Symbol& dependences_function)
    {
    }
    void TaskProperties::create_copies_function(
            /* out */
            TL::Symbol& copies_function)
    {
    }

    void TaskProperties::capture_environment(
            /* out */
            Nodecl::NodeclBase& capture_env)
    {
    }

} }
