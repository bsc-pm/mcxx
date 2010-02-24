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
#include "tl-devices.hpp"

using namespace TL;
using namespace TL::Nanox;

Source TL::Nanox::common_parallel_code(const std::string& outline_name, 
        const std::string& struct_arg_type_name,
        Source num_threads,
        ScopeLink sl,
        const DataEnvironInfo& data_environ_info,
        AST_t parallel_code,
        const ObjectList<std::string>& current_targets)
{
    Source result;

    Source fill_outline_arguments, fill_immediate_arguments;

    fill_data_args(
            "ol_args",
            data_environ_info, 
            ObjectList<OpenMP::DependencyItem>(), // empty
            /* is_pointer */ true,
            fill_outline_arguments);

    bool immediate_is_alloca = false;
    bool env_is_runtime_sized = data_environ_info.environment_is_runtime_sized();

    if (env_is_runtime_sized)
    {
        immediate_is_alloca = true;
    }

    fill_data_args(
            "imm_args",
            data_environ_info, 
            ObjectList<OpenMP::DependencyItem>(), // empty
            /* is_pointer */ immediate_is_alloca,
            fill_immediate_arguments);

    Source device_descriptor, 
           device_description, 
           device_description_line, 
           num_devices,
           ancillary_device_description;
    device_descriptor << outline_name << "_devices";
    device_description
        << ancillary_device_description
        << "nanos_device_t " << device_descriptor << "[] ="
        << "{"
        << device_description_line
        << "};"
        ;

    DeviceHandler &device_handler = DeviceHandler::get_device_handler();
    for (ObjectList<std::string>::const_iterator it = current_targets.begin();
            it != current_targets.end();
            it++)
    {
        DeviceProvider* device_provider = device_handler.get_device(*it);

        if (device_provider == NULL)
        {
            internal_error("invalid device '%s'\n",
                    it->c_str());
        }

        OutlineFlags outline_flags;

        outline_flags.leave_team = true;
        outline_flags.barrier_at_end = true;

        device_provider->create_outline(outline_name,
                struct_arg_type_name,
                data_environ_info,
                outline_flags,
                sl,
                parallel_code);

        device_provider->get_device_descriptor(outline_name, 
                data_environ_info, 
                outline_flags,
                ancillary_device_description, 
                device_description_line);
    }

    num_devices << current_targets.size();

    Source struct_runtime_size, struct_size;
    Source immediate_decl;

    if (!immediate_is_alloca)
    {
        immediate_decl
            << struct_arg_type_name << " imm_args;"
            ;
    }
    else
    {
        Source alloca_size;
        immediate_decl 
            << struct_arg_type_name << " * __restrict imm_args = (" << struct_arg_type_name << "*) __builtin_alloca(" << struct_size << ");"
            ;
    }

    Scope scope = sl.get_scope(parallel_code);

    if (env_is_runtime_sized)
    {
        struct_runtime_size
            << "int struct_runtime_size = "
            << "sizeof(" << struct_arg_type_name << ") + "
            << data_environ_info.sizeof_variable_part(scope)
            << ";"
            ;
        struct_size
            << "struct_runtime_size" 
            ;
    }
    else
    {
        struct_size
            << "sizeof("  << struct_arg_type_name << ")"
            ;
    }

    // FIXME - This will be meaningful with 'copy_in' and 'copy_out'
    Source num_copies, copy_data;
    num_copies << "0";
    copy_data << "(nanos_copy_data_t*)0";

    result
        << "{"
        <<   "unsigned int _nanos_num_threads = " << num_threads << ";"
        <<   "nanos_team_t _nanos_team = (nanos_team_t)0;"
        <<   "nanos_thread_t _nanos_threads[_nanos_num_threads];"
        <<   "nanos_err_t err;"
        <<   "err = nanos_create_team(&_nanos_team, (nanos_sched_t)0, &_nanos_num_threads,"
        <<              "(nanos_constraint_t*)0, /* reuse_current */ 1, _nanos_threads);"
        <<   "if (err != NANOS_OK) nanos_handle_error(err);"

        <<   device_description      

        <<   struct_runtime_size

        <<   "nanos_wd_props_t props = { 0 };"
        <<   "props.mandatory_creation = 1;"
        <<   "int _i;"
        <<   "for (_i = 1; _i < _nanos_num_threads; _i++)"
        <<   "{"
        //   We have to create a wd tied to a thread
        <<      struct_arg_type_name << " *ol_args;"
        <<      "props.tie_to = _nanos_threads[_i];"
        <<      "nanos_wd_t wd = 0;"
        <<      "err = nanos_create_wd(&wd, " << num_devices << ","
        <<                    device_descriptor << ", "
        <<                    struct_size << ","
        <<                    "(void**)&ol_args,"
        <<                    "nanos_current_wd(), "
        <<                    "&props, " << num_copies << "," << copy_data << ");"
        <<      "if (err != NANOS_OK) nanos_handle_error(err);"
        <<      fill_outline_arguments
        <<      "err = nanos_submit(wd, 0, (nanos_dependence_t*)0, 0);"
        <<      "if (err != NANOS_OK) nanos_handle_error(err);"
        <<   "}"
        <<   "props.tie_to = &_nanos_threads[0];"
        <<   immediate_decl
        <<   fill_immediate_arguments
        <<   "nanos_create_wd_and_run(" << num_devices << ", "
        <<                              device_descriptor << ", "
        <<                              struct_size << ", " << (immediate_is_alloca ? "imm_args" : "&imm_args") << ","
        <<                              "0,"
        <<                              "(nanos_dependence_t*)0, "
        <<                              "&props, " << num_copies << "," << copy_data << ");"
        <<   "nanos_end_team(_nanos_team);"
        << "}"
        ;

    return result;
}
