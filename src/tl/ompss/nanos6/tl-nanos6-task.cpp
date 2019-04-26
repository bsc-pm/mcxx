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
#include "tl-nanos6-fortran-support.hpp"
#include "tl-nanos6-interface.hpp"
#include "tl-nanos6-support.hpp"

#include "tl-counters.hpp"
#include "tl-source.hpp"

#include "cxx-exprtype.h"

namespace TL { namespace Nanos6 {

    void Lower::visit(const Nodecl::OpenMP::Task& node)
    {
        Nodecl::OpenMP::Task task = node;

        walk(task.get_statements());
        Nodecl::NodeclBase serial_stmts;

        // If disabled, act normally
        if (!_phase->_final_clause_transformation_disabled)
        {
            std::map<Nodecl::NodeclBase, Nodecl::NodeclBase>::iterator it = _final_stmts_map.find(task);
            ERROR_CONDITION(it == _final_stmts_map.end(), "Invalid serial statements", 0);
            serial_stmts = it->second;
        }

        lower_task(task, serial_stmts);
    }

    namespace {

        // Substitute the task node for an ifelse for when using final
        void create_final_if_else_statement(Nodecl::OpenMP::Task& node, Nodecl::NodeclBase& serial_stmts_placeholder)
        {

            Nodecl::NodeclBase stmts = node.get_statements();

            // Wrap the function call into if (nanos_in_final())
            TL::Symbol nanos_in_final_sym = get_nanos6_function_symbol("nanos6_in_final");

            Nodecl::NodeclBase call_to_nanos_in_final = Nodecl::FunctionCall::make(
                /* called */ nanos_in_final_sym.make_nodecl(/* set_ref_type */ true, node.get_locus()),
                /* arguments */ Nodecl::NodeclBase::null(),
                /* alternate_name */Nodecl::NodeclBase::null(),
                /* function_form */ Nodecl::NodeclBase::null(),
                TL::Type::get_int_type());

            Nodecl::OpenMP::Task new_task = Nodecl::OpenMP::Task::make(node.get_environment(), stmts, node.get_locus());

            Scope sc = node.retrieve_context();
            Scope not_final_context = new_block_context(sc.get_decl_context());

            Nodecl::NodeclBase not_final_compound_stmt = Nodecl::Context::make(
                Nodecl::List::make(
                    Nodecl::CompoundStatement::make(
                        Nodecl::List::make(new_task),
                        /* finally */ Nodecl::NodeclBase::null(),
                        node.get_locus())),
                not_final_context,
                node.get_locus());

            serial_stmts_placeholder = Nodecl::EmptyStatement::make();

            Nodecl::NodeclBase if_in_final = Nodecl::IfElseStatement::make(
                    Nodecl::Different::make(
                        call_to_nanos_in_final,
                        const_value_to_nodecl_with_basic_type(
                            const_value_get_signed_int(0),
                            get_size_t_type()),
                        get_bool_type()),
                    Nodecl::List::make(serial_stmts_placeholder),
                    Nodecl::List::make(not_final_compound_stmt)
                );

            node.replace(if_in_final);

            node = new_task;
        }

        // Creates the task instantiation and submission
        void handle_task_transformation(const Nodecl::OpenMP::Task& node, TaskProperties& task_properties)
        {
            Nodecl::NodeclBase args_size;
            TL::Type data_env_struct;
            bool requires_initialization;
            task_properties.create_environment_structure(
                    /* out */
                    data_env_struct,
                    args_size,
                    requires_initialization);

            TL::Symbol task_invocation_info;
            task_properties.create_task_invocation_info(
                    /* out */
                    task_invocation_info);

            TL::Symbol implementations;
            task_properties.create_task_implementations_info(
                    /* out */
                    implementations);

            TL::Symbol task_info;
            task_properties.create_task_info(
                    implementations,
                    /* out */
                    task_info);

            TL::Scope sc = node.retrieve_context();

            TL::Symbol args;
            {
                TL::Counter &counter = TL::CounterManager::get_counter("nanos6-task-args");
                std::stringstream ss;
                ss << "nanos_data_env_" << (int)counter;
                counter++;

                args = sc.new_symbol(ss.str());
                args.get_internal_symbol()->kind = SK_VARIABLE;
                args.set_type(data_env_struct.get_pointer_to());
                symbol_entity_specs_set_is_user_declared(args.get_internal_symbol(), 1);
            }

            TL::Symbol task_ptr;
            {
                TL::Counter &counter = TL::CounterManager::get_counter("nanos6-task-ptr");
                std::stringstream ss;
                ss << "nanos_task_ptr_" << (int)counter;
                counter++;

                task_ptr = sc.new_symbol(ss.str());
                task_ptr.get_internal_symbol()->kind = SK_VARIABLE;
                task_ptr.set_type(TL::Type::get_void_type().get_pointer_to());
                symbol_entity_specs_set_is_user_declared(task_ptr.get_internal_symbol(), 1);
            }

            Nodecl::List new_stmts;

            // In Fortran we have to initialize the pointer to the data environment
            if (IS_FORTRAN_LANGUAGE)
            {
                TL::Symbol intrinsic_null = get_fortran_intrinsic_symbol<0>("null", Nodecl::List(), /* is_call */ 0);

                new_stmts.append(
                        Nodecl::ExpressionStatement::make(
                            Nodecl::Assignment::make(
                                args.make_nodecl(/*set_ref_type*/true),
                                Nodecl::FunctionCall::make(
                                    intrinsic_null.make_nodecl(/*set_ref_type*/ true),
                                    /* arguments      */ Nodecl::NodeclBase::null(),
                                    /* alternate-name */ Nodecl::NodeclBase::null(),
                                    /* function-form  */ Nodecl::NodeclBase::null(),
                                    intrinsic_null.get_type().returns(),
                                    node.get_locus()),
                                args.get_type())));
            }

                    // Create task
            {
                if (IS_CXX_LANGUAGE)
                {
                    new_stmts.append(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), args));
                    new_stmts.append(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), task_ptr));
                }

                TL::Symbol nanos_create_task_sym = get_nanos6_function_symbol("nanos6_create_task");

                // void nanos_create_task(
                //         nanos_task_info *task_info,
                //         nanos_task_invocation_info *task_invocation_info,
                //         size_t args_block_size,
                //         /* OUT */ void **args_block_pointer,
                //         /* OUT */ void **task_pointer,
                //         size_t flags,
                //         size_t num_deps);

                Nodecl::List create_task_args;

                // &task_info
                Nodecl::NodeclBase task_info_ptr =
                    Nodecl::Reference::make(
                        task_info.make_nodecl(
                            /* set_ref_type */ true,
                            node.get_locus()),
                        task_info.get_type().get_pointer_to(),
                        node.get_locus());

                create_task_args.append(task_info_ptr);


                // &task_invocation_info
                Nodecl::NodeclBase task_invocation_info_ptr =
                    Nodecl::Reference::make(
                            task_invocation_info.make_nodecl(
                                /* set_ref_type */ true,
                                node.get_locus()),
                            task_invocation_info.get_type().get_pointer_to(),
                            node.get_locus());

                create_task_args.append(task_invocation_info_ptr);


                //args_size
                create_task_args.append(args_size);


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
                create_task_args.append(args_ptr_out);


                // &task_ptr
                Nodecl::NodeclBase task_ptr_out =
                    Nodecl::Reference::make(
                            task_ptr.make_nodecl(
                                /* set_ref_type */ true,
                                node.get_locus()),
                            task_ptr.get_type().get_pointer_to(),
                            node.get_locus());

                create_task_args.append(task_ptr_out);

                // task_flags
                {
                    TL::Symbol task_flags;
                    {
                        TL::Counter &counter = TL::CounterManager::get_counter("nanos6-task-flags");
                        std::stringstream ss;
                        ss << "task_flags_" << (int)counter;
                        counter++;

                        task_flags = sc.new_symbol(ss.str());
                        task_flags.get_internal_symbol()->kind = SK_VARIABLE;
                        task_flags.get_internal_symbol()->type_information = TL::Type::get_size_t_type().get_internal_type();
                        symbol_entity_specs_set_is_user_declared(task_flags.get_internal_symbol(), 1);
                    }

                    if (IS_CXX_LANGUAGE)
                        new_stmts.append(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), task_flags));

                    Nodecl::NodeclBase task_flags_stmts;
                    task_properties.compute_task_flags(task_flags, task_flags_stmts);
                    new_stmts.append(task_flags_stmts);

                    create_task_args.append(task_flags.make_nodecl(/*set_ref_type */ true));
                }

                // num_deps
                {
                    TL::Symbol num_deps;
                    {
                        TL::Counter &counter = TL::CounterManager::get_counter("nanos6-num-dependences");
                        std::stringstream ss;
                        ss << "nanos_num_deps_" << (int)counter;
                        counter++;

                        num_deps = sc.new_symbol(ss.str());
                        num_deps.get_internal_symbol()->kind = SK_VARIABLE;
                        num_deps.set_type(TL::Type::get_size_t_type());
                        symbol_entity_specs_set_is_user_declared(num_deps.get_internal_symbol(), 1);
                    }

                    Nodecl::NodeclBase compute_num_deps_stmts;
                    task_properties.compute_number_of_dependences(num_deps, sc, /* out */ compute_num_deps_stmts);

                    new_stmts.append(Nodecl::ObjectInit::make(num_deps));
                    if (IS_CXX_LANGUAGE)
                        new_stmts.append(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), num_deps));

                    new_stmts.append(compute_num_deps_stmts);

                    create_task_args.append(num_deps.make_nodecl(/* set_ref_type */ true));
                }

            if (IS_FORTRAN_LANGUAGE)
            {
                Nodecl::NodeclBase allocate_stmt = Nodecl::FortranAllocateStatement::make(
                            Nodecl::List::make(args.make_nodecl(/*set_ref_type*/ true)),
                                /* options */ Nodecl::NodeclBase::null(),
                                /* alloc-type */ Nodecl::NodeclBase::null());

                new_stmts.append(allocate_stmt);
            }

                Nodecl::NodeclBase call_to_nanos_create_task =
                    Nodecl::ExpressionStatement::make(
                            Nodecl::FunctionCall::make(
                                nanos_create_task_sym.make_nodecl(/* set_ref_type */ true, node.get_locus()),
                                create_task_args,
                                /* alternate name */ Nodecl::NodeclBase::null(),
                                /* function form  */ Nodecl::NodeclBase::null(),
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
                        sc,
                        /* out */ capture_env);

                new_stmts.append(capture_env);
            }

            if (task_properties.task_is_loop())
            {
                TL::Symbol nanos_register_loop_sym = get_nanos6_function_symbol("nanos6_register_taskloop_bounds");

                ERROR_CONDITION(!node.get_statements().is<Nodecl::List>(), "Unexpected node\n", 0);
                Nodecl::NodeclBase stmt = node.get_statements().as<Nodecl::List>().front();
                ERROR_CONDITION(!stmt.is<Nodecl::Context>(), "Unexpected node\n", 0);
                stmt = stmt.as<Nodecl::Context>().get_in_context().as<Nodecl::List>().front();
                ERROR_CONDITION(!stmt.is<Nodecl::ForStatement>(), "Unexpected node\n", 0);

                TL::ObjectList<TL::Symbol> params = nanos_register_loop_sym.get_related_symbols();
                Nodecl::List reg_loop_args;

                reg_loop_args.append(task_ptr.make_nodecl(/*ref_type*/true));

                Nodecl::NodeclBase lower_bound = task_properties.get_lower_bound().shallow_copy();
                if (IS_FORTRAN_LANGUAGE)
                    lower_bound = Nodecl::Conversion::make(lower_bound, params[1].get_type());
                reg_loop_args.append(lower_bound);

                Nodecl::NodeclBase upper_bound = task_properties.get_upper_bound().shallow_copy();
                if (IS_FORTRAN_LANGUAGE)
                    upper_bound = Nodecl::Conversion::make(upper_bound, params[2].get_type());
                reg_loop_args.append(upper_bound);

                Nodecl::NodeclBase step = task_properties.get_step().shallow_copy();
                if (IS_FORTRAN_LANGUAGE)
                    step = Nodecl::Conversion::make(step, params[3].get_type());
                reg_loop_args.append(step);

                Nodecl::NodeclBase chunksize = task_properties.get_chunksize().shallow_copy();
                if (IS_FORTRAN_LANGUAGE)
                    chunksize = Nodecl::Conversion::make(chunksize, params[4].get_type());
                reg_loop_args.append(chunksize);

                new_stmts.append(
                    Nodecl::ExpressionStatement::make(
                        Nodecl::FunctionCall::make(
                            nanos_register_loop_sym.make_nodecl(/*ref_type*/true),
                            reg_loop_args,
                            /* alternate symbol */ Nodecl::NodeclBase::null(),
                            /* function form */ Nodecl::NodeclBase::null(),
                            TL::Type::get_void_type(),
                            node.get_locus()),
                        node.get_locus()));
            }

            // Submit the created task
            {
                TL::Symbol nanos_submit_task_sym = get_nanos6_function_symbol("nanos6_submit_task");

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

    }

    // This function is only called when lowering a TaskWait with dependences
    void Lower::lower_task(const Nodecl::OpenMP::Task& node)
    {
        Nodecl::NodeclBase dummy_serial_stmts;
        TaskProperties task_properties(node, dummy_serial_stmts, _phase, this);
        handle_task_transformation(node, task_properties);
    }

    void Lower::lower_task(const Nodecl::OpenMP::Task& node, const Nodecl::NodeclBase& serial_stmts)
    {
        ERROR_CONDITION(!_phase->_final_clause_transformation_disabled
                && serial_stmts.is_null(),
                "Invalid serial statements for a task", 0);

        Nodecl::OpenMP::Task task = node;
        Nodecl::NodeclBase final_stmts;
        Nodecl::NodeclBase serial_stmts_placeholder;

        if (!_phase->_final_clause_transformation_disabled)
        {
            Scope in_final_scope =
                new_block_context(task.retrieve_context().get_decl_context());

            final_stmts = Nodecl::Context::make(
                    Nodecl::List::make(
                        Nodecl::CompoundStatement::make(
                            serial_stmts,
                            /* finally */ Nodecl::NodeclBase::null(),
                            task.get_locus())),
                    in_final_scope,
                    task.get_locus());

            create_final_if_else_statement(task, serial_stmts_placeholder);
        }

        TaskProperties task_properties(task, final_stmts, _phase, this);
        handle_task_transformation(task, task_properties);

        if (!_phase->_final_clause_transformation_disabled)
        {
            serial_stmts_placeholder.replace(final_stmts);

            // Traverse the serial statements since they may contain additional pragmas
            walk(final_stmts);
        }
    }

} }
