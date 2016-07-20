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

#include "cxx-exprtype.h"

namespace TL { namespace Nanos6 {

    void Lower::visit(const Nodecl::OpenMP::Task& node)
    {
        walk(node.get_statements());
        Nodecl::NodeclBase serial_stmts;

        // If disabled, act normally
        if (!_phase->_final_clause_transformation_disabled)
        {
            std::map<Nodecl::NodeclBase, Nodecl::NodeclBase>::iterator it = _final_stmts_map.find(node);
            ERROR_CONDITION(it == _final_stmts_map.end(), "Invalid serial statements", 0);
            serial_stmts = it->second;
        }

        lower_task(node, serial_stmts);
    }

    // Substitute the task node for an ifelse for when using final
    void Lower::lower_task(const Nodecl::OpenMP::Task& node, Nodecl::NodeclBase& serial_stmts)
    {
        ERROR_CONDITION(serial_stmts.is_null()
                && !_phase->_final_clause_transformation_disabled,
                "Invalid serial statement for a task", 0);

        Nodecl::OpenMP::Task new_task = node;

        if (!_phase->_final_clause_transformation_disabled)
        {
            // Traverse the serial statements since they may contain additional pragmas
            walk(serial_stmts);

            Nodecl::NodeclBase stmts = node.get_statements();

            // Wrap the function call into if (nanos_in_final())
            TL::Symbol nanos_in_final_sym =
                TL::Scope::get_global_scope().get_symbol_from_name("nanos_in_final");
            ERROR_CONDITION(!nanos_in_final_sym .is_valid()
                    || !nanos_in_final_sym.is_function(),
                    "Invalid symbol", 0);

            Nodecl::NodeclBase call_to_nanos_in_final = Nodecl::FunctionCall::make(
                nanos_in_final_sym.make_nodecl(/* set_ref_type */ true,
                    node.get_locus()), /* called */
                Nodecl::NodeclBase::null(), /* Argument list */
                Nodecl::NodeclBase::null(), /* Alternate name */
                Nodecl::NodeclBase::null(), /* Function Form */
                TL::Type::get_int_type()
            );

            new_task = Nodecl::OpenMP::Task::make(node.get_environment(), stmts);

            Scope sc = node.retrieve_context();
            Scope not_final_context = new_block_context(sc.get_decl_context());

            Nodecl::NodeclBase not_final_compound_stmt = Nodecl::Context::make(
                Nodecl::List::make(
                    Nodecl::CompoundStatement::make(
                        Nodecl::List::make(new_task),
                        /* finally */ Nodecl::NodeclBase::null(),
                        node.get_locus()
                        )
                    ),
                not_final_context,
                node.get_locus()
            );

            Scope in_final_context = new_block_context(sc.get_decl_context());
            Nodecl::NodeclBase in_final_compound_stmts = Nodecl::Context::make(
                Nodecl::List::make(
                    Nodecl::CompoundStatement::make(
                        serial_stmts,
                        /* finally */ Nodecl::NodeclBase::null(),
                        node.get_locus()
                        )
                    ),
                in_final_context,
                node.get_locus()
            );

            Nodecl::NodeclBase if_in_final = Nodecl::IfElseStatement::make(
                    Nodecl::Different::make(
                        call_to_nanos_in_final,
                        const_value_to_nodecl_with_basic_type(
                            const_value_get_signed_int(0),
                            get_size_t_type()),
                        get_bool_type()),
                    Nodecl::List::make(in_final_compound_stmts),
                    Nodecl::List::make(not_final_compound_stmt)
                );

            node.replace(if_in_final);
        }

        lower_task(new_task);
    }

    // Creates the task instantiation and submission
    void Lower::lower_task(const Nodecl::OpenMP::Task& node)
    {
        TaskProperties task_properties = TaskProperties::gather_task_properties(_phase, node);

        Nodecl::NodeclBase args_size;
        TL::Type data_env_struct;
        task_properties.create_environment_structure(
                /* out */
                data_env_struct,
                args_size);

        TL::Symbol task_info, task_invocation_info;
        Nodecl::NodeclBase local_init_task_info;
        task_properties.create_task_info(
                /* out */
                task_info,
                task_invocation_info,
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
            if (IS_CXX_LANGUAGE)
            {
                new_stmts.append(
                        Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), args));
                new_stmts.append(
                        Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), task_ptr));
            }

            TL::Symbol nanos_create_task_sym =
                TL::Scope::get_global_scope().get_symbol_from_name("nanos_create_task");
            ERROR_CONDITION(!nanos_create_task_sym.is_valid()
                    || !nanos_create_task_sym.is_function(),
                    "Invalid symbol", 0);

            // Source new_task_src;
            // new_task_src
            //     << "nanos_create_task("
            //     <<    "&" << as_symbol(task_info) << ","
            //     <<    as_expression(args_size) << ","
            //     /* out */
            //     <<    "(void**)&" << as_symbol(args) << ","
            //     <<    "&" << as_symbol(task_ptr) << ");"
            //     ;

            // &task_info
            Nodecl::NodeclBase task_info_ptr =
                Nodecl::Reference::make(
                    task_info.make_nodecl(
                        /* set_ref_type */ true,
                        node.get_locus()),
                    task_info.get_type().get_pointer_to(),
                    node.get_locus());

            // (void**)&args
            Nodecl::NodeclBase cast;
            Nodecl::NodeclBase args_ptr_out =
                cast = Nodecl::Conversion::make(
                        Nodecl::Reference::make(
                            args.make_nodecl(
                                /* set_ref_type */ true,
                                node.get_locus()),
                            args.get_type().get_pointer_to(),
                            node.get_locus()),
                        TL::Type::get_void_type().get_pointer_to().get_pointer_to(),
                        node.get_locus());

            cast.set_text("C");

            // &task_ptr
            Nodecl::NodeclBase task_ptr_out =
                Nodecl::Reference::make(
                        task_ptr.make_nodecl(
                            /* set_ref_type */ true,
                            node.get_locus()),
                        task_ptr.get_type().get_pointer_to(),
                        node.get_locus());

            Nodecl::NodeclBase task_invocation_info_ptr =
                Nodecl::Reference::make(
                        task_invocation_info.make_nodecl(
                            /* set_ref_type */ true,
                            node.get_locus()),
                        task_invocation_info.get_type().get_pointer_to(),
                        node.get_locus());

            Nodecl::NodeclBase call_to_nanos_create_task;
            Nodecl::NodeclBase flags_nodecl;

            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                flags_nodecl = task_properties.create_task_flags(/* Empty */ TL::Symbol());
            }
            else // if (IS_FORTRAN_LANGUAGE)
            {
                // Add a piece of code before calling nanos_create_task, that will set up the flags
                std::string task_flags_name;
                {
                    TL::Counter &counter = TL::CounterManager::get_counter("nanos6-task-flags");
                    std::stringstream ss;
                    ss << "flags_" << (int)counter;
                    counter++;
                    task_flags_name = ss.str();
                }

                TL::Symbol task_flags = sc.new_symbol(task_flags_name);
                task_flags.get_internal_symbol()->kind = SK_VARIABLE;
                task_flags.set_type(TL::Type::get_size_t_type());
                symbol_entity_specs_set_is_user_declared(
                        task_flags.get_internal_symbol(), 1);
                flags_nodecl = Nodecl::Symbol::make(task_flags, node.get_locus());

                Nodecl::NodeclBase flag_setter = task_properties.create_task_flags(task_flags);
                new_stmts.append(flag_setter);
            }

            call_to_nanos_create_task =
                Nodecl::ExpressionStatement::make(
                        Nodecl::FunctionCall::make(
                            nanos_create_task_sym.make_nodecl(/* set_ref_type */ true,
                                node.get_locus()),
                            Nodecl::List::make(
                                task_info_ptr,
                                task_invocation_info_ptr,
                                args_size,
                                /* out */
                                args_ptr_out,
                                task_ptr_out,
                                /* Flags */
                                flags_nodecl),
                            /* alternate symbol */ Nodecl::NodeclBase::null(),
                            /* function form */ Nodecl::NodeclBase::null(),
                            TL::Type::get_void_type(),
                            node.get_locus()),
                        node.get_locus());

            new_stmts.append(call_to_nanos_create_task);
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
            TL::Symbol nanos_submit_task_sym =
                TL::Scope::get_global_scope().get_symbol_from_name("nanos_submit_task");
            ERROR_CONDITION(!nanos_submit_task_sym.is_valid()
                    || !nanos_submit_task_sym.is_function(),
                    "Invalid symbol", 0);

            Nodecl::NodeclBase task_ptr_arg = task_ptr.make_nodecl(/* set_ref_type */ true);
            task_ptr_arg = ::cxx_nodecl_make_conversion(
                    task_ptr_arg.get_internal_nodecl(),
                    task_ptr.get_type().no_ref().get_internal_type(),
                    TL::Scope::get_global_scope().get_decl_context(),
                    node.get_locus());

            Nodecl::NodeclBase new_task =
                Nodecl::ExpressionStatement::make(
                        Nodecl::FunctionCall::make(
                            nanos_submit_task_sym.make_nodecl(/* set_ref_type */ true),
                            Nodecl::List::make(task_ptr_arg),
                            /* alternate symbol */ Nodecl::NodeclBase::null(),
                            /* function form */ Nodecl::NodeclBase::null(),
                            TL::Type::get_void_type(),
                            node.get_locus()),
                        node.get_locus());

            new_stmts.append(new_task);
        }
        node.replace(new_stmts);
    }

} }
