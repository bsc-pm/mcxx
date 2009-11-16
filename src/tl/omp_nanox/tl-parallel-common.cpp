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

#include "tl-parallel-common.hpp"

using namespace TL;
using namespace TL::Nanox;

Source TL::Nanox::common_parallel_spawn_code(Source num_devices,
        Source device_descriptor, 
        Source struct_arg_type_name,
        Source num_threads,
        const DataEnvironInfo& data_environ_info)
{
    Source result;

    Source fill_outline_arguments, fill_immediate_arguments;

    fill_data_args("ol_args->", data_environ_info, fill_outline_arguments);
    fill_data_args("imm_args.", data_environ_info, fill_immediate_arguments);

    result
        << "{"
        // FIXME - How to get the default number of threads?
        <<   "unsigned int _nanos_num_threads = " << num_threads << ";"
        <<   "nanos_team_t _nanos_team;"
        <<   "nanos_thread_t _nanos_threads[_nanos_num_threads];"
        <<   "nanos_err_t err;"
        <<   "err = nanos_create_team(&_nanos_team, (nanos_sched_t)0, &_nanos_num_threads,"
        <<              "(nanos_constraint_t*)0, /* reuse */ 0, _nanos_threads);"
        <<   "if (err != NANOS_OK) nanos_handle_error(err);"

        <<   "nanos_wd_props_t props = { 0 };"
        <<   "props.mandatory_creation = 1;"
        <<   "int _i;"
        <<   "for (_i = 1; _i < _nanos_num_threads; _i++)"
        <<   "{"
        //   We have to create a wd tied to a thread
        <<      struct_arg_type_name << " *ol_args;"
        <<      "props.tie_to = &_nanos_threads[_i];"
        <<      "nanos_wd_t wd;"
        <<      "err = nanos_create_wd(&wd, " << num_devices << ","
        <<                    device_descriptor << ", "
        <<                    "sizeof(" << struct_arg_type_name << "),"
        <<                    "(void**)&ol_args,"
        <<                    "nanos_current_wd(), "
        <<                    "&props);"
        <<      "if (err != NANOS_OK) nanos_handle_error(err);"
        <<      fill_outline_arguments
        <<      "nanos_submit(wd, (nanos_dependence_t*)0, _nanos_team);"
        <<   "}"
        <<   "props.tie_to = &_nanos_threads[0];"
        <<   struct_arg_type_name << " imm_args;"
        <<   fill_immediate_arguments
        <<   "nanos_create_wd_and_run(" << num_devices << ", "
        <<                              device_descriptor << ", "
        <<                              "&imm_args,"
        <<                              "(nanos_dependence_t*)0, "
        <<                              "&props);"
        //   The ending barrier will be in the outlined function
        << "}"
        ;

    return result;
}
