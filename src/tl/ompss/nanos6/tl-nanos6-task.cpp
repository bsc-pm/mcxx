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


#include "tl-nanos6-lower.hpp"
#include "tl-nanos6-task-properties.hpp"
#include "tl-counters.hpp"
#include "tl-source.hpp"

namespace TL { namespace Nanos6 {

    void Lower::visit(const Nodecl::OpenMP::Task& node)
    {
        TaskProperties task_properties = TaskProperties::gather_task_properties(node);

        Nodecl::NodeclBase args_size;
        TL::Type data_env_struct;
        task_properties.create_environment_structure(
                /* out */
                data_env_struct,
                args_size);

        TL::Symbol task_info;
        Nodecl::NodeclBase local_init_task_info;
        task_properties.create_task_info(
                /* out */
                task_info,
                local_init_task_info);

        TL::Scope sc = node.retrieve_context();

        std::string args_name;
        {
            TL::Counter &counter = TL::CounterManager::get_counter("nanos6-task-args");
            std::stringstream ss;
            ss << "nanos_data_env_" << (int)counter;
            counter++;
            args_name = ss.str();
        }

        TL::Symbol args = sc.new_symbol(args_name);
        args.get_internal_symbol()->kind = SK_VARIABLE;
        args.set_type(data_env_struct.get_pointer_to());
        symbol_entity_specs_set_is_user_declared(
                args.get_internal_symbol(),
                1);

        std::string task_ptr_name;
        {
            TL::Counter &counter = TL::CounterManager::get_counter("nanos6-task-ptr");
            std::stringstream ss;
            ss << "nanos_task_ptr_" << (int)counter;
            counter++;
            task_ptr_name = ss.str();
        }
        TL::Symbol task_ptr = sc.new_symbol(task_ptr_name);
        task_ptr.get_internal_symbol()->kind = SK_VARIABLE;
        task_ptr.set_type(TL::Type::get_void_type().get_pointer_to());
        symbol_entity_specs_set_is_user_declared(
                task_ptr.get_internal_symbol(), 1);

        Nodecl::List new_stmts;
        if (!local_init_task_info.is_null())
        {
            // Init task info if it happens to be local
            new_stmts.append(local_init_task_info);
        }

        // Create task
        {
            Source new_task_src;
            new_task_src
                << "nanos_create_task("
                <<    "&" << as_symbol(task_info) << ","
                <<    as_expression(args_size) << ","
                /* out */
                <<    "(void**)&" << as_symbol(args) << ","
                <<    "&" << as_symbol(task_ptr) << ");"
                ;
            Nodecl::NodeclBase new_task = new_task_src.parse_statement(node);
            new_stmts.append(new_task);
        }

        // Capture environment
        {
            Nodecl::NodeclBase capture_env;
            task_properties.capture_environment(
                    args,
                    /* out */ capture_env);

            new_stmts.append(capture_env);
        }

        // Submit the created task
        {
            Source nanos_submit_src;
            nanos_submit_src
                << "nanos_submit_task(" << as_symbol(task_ptr) << ");"
                ;

            Nodecl::NodeclBase new_task = nanos_submit_src.parse_statement(node);
            new_stmts.append(new_task);
        }

        node.replace(new_stmts);
    }

} }
