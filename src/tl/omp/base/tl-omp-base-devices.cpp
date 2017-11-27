/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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


#include "tl-omp-base.hpp"
#include "cxx-diagnostic.h"
#include "cxx-cexpr.h"

namespace TL { namespace OpenMP {

    namespace {
        const decl_context_t* decl_context_map_id(const decl_context_t* d)
        {
            return d;
        }

        Nodecl::NodeclBase handle_device_clause(
                TL::PragmaCustomClause &device,
                const locus_t* locus)
        {
            TL::ObjectList<Nodecl::NodeclBase> expr_list;
            if (device.is_defined())
            {
                expr_list = device.get_arguments_as_expressions();
            }

            Nodecl::NodeclBase device_id_expr;
            if (expr_list.empty())
            {
                // Our default device is 0
                device_id_expr = const_value_to_nodecl(const_value_get_signed_int(0));
                if (device.is_defined())
                {
                    error_printf_at(locus, "empty 'device' clause\n");
                }
            }
            else
            {
                device_id_expr = expr_list[0];
                if (expr_list.size() > 1)
                {
                    error_printf_at(expr_list[1].get_locus(),
                            "too many expressions in 'device' clause\n");
                }
            }
            ERROR_CONDITION(device_id_expr.is_null(), "Expecting a valid device id here", 0);

            if (!::is_integer_type(no_ref(device_id_expr.get_type().get_internal_type())))
            {
                error_printf_at(device_id_expr.get_locus(),
                        "expression of 'device' must be an integer-expression\n");
            }

            return Nodecl::OpenMP::Device::make(device_id_expr, device_id_expr.get_locus());
        }

        Nodecl::NodeclBase handle_device_clause(TL::PragmaCustomLine &ctr)
        {
            TL::PragmaCustomClause device = ctr.get_clause("device");
            return handle_device_clause(device, ctr.get_locus());
        }

        Nodecl::NodeclBase handle_if_clause(TL::PragmaCustomClause &if_clause,
                const locus_t* locus)
        {
            if (!if_clause.is_defined())
                return Nodecl::NodeclBase::null();

            TL::ObjectList<Nodecl::NodeclBase> expr_list = if_clause.get_arguments_as_expressions();

            Nodecl::NodeclBase result;
            if (expr_list.empty())
            {
                error_printf_at(locus, "expecting expression in 'if' clause\n");
            }
            else
            {
                if (expr_list.size() > 1)
                {
                    error_printf_at(expr_list[1].get_locus(), "too many expressions in 'if' clause\n");
                }
                result = Nodecl::OpenMP::If::make(
                        expr_list[0],
                        expr_list[0].get_locus());
            }

            return result;
        }

        Nodecl::NodeclBase handle_if_clause(TL::PragmaCustomLine& ctr)
        {
            TL::PragmaCustomClause if_clause = ctr.get_clause("if");
            return handle_if_clause(if_clause, ctr.get_locus());
        }

        void handle_target_data_clauses(TL::PragmaCustomStatement &ctr,
                // out
                Nodecl::NodeclBase &device_id,
                Nodecl::NodeclBase &if_clause)
        {
            TL::PragmaCustomLine line = ctr.get_pragma_line();

            device_id = handle_device_clause(line);
            if_clause = handle_if_clause(line);
        }

        template <typename T>
            Nodecl::NodeclBase make_map_node(Nodecl::NodeclBase n)
            {
                return T::make(n, n.get_locus());
            }

        Nodecl::NodeclBase make_device_data_environment(OpenMP::DataEnvironment& data_environment)
        {
            TL::ObjectList<OpenMP::MappingValue> all_device_mappings = data_environment.get_all_device_mappings();
            struct Filter
            {
                static bool is_map_to(const OpenMP::MappingValue& m) { return m.direction == OpenMP::MAP_DIR_TO; }
                static bool is_map_from(const OpenMP::MappingValue& m) { return m.direction == OpenMP::MAP_DIR_FROM; }
                static bool is_map_tofrom(const OpenMP::MappingValue& m) { return m.direction == OpenMP::MAP_DIR_TOFROM; }
                static bool is_map_alloc(const OpenMP::MappingValue& m) { return m.direction == OpenMP::MAP_DIR_ALLOC; }
            };

            TL::ObjectList<OpenMP::MappingValue> mappings_to =
                all_device_mappings.filter(Filter::is_map_to);
            TL::ObjectList<OpenMP::MappingValue> mappings_from =
                all_device_mappings.filter(Filter::is_map_from);
            TL::ObjectList<OpenMP::MappingValue> mappings_tofrom =
                all_device_mappings.filter(Filter::is_map_tofrom);
            TL::ObjectList<OpenMP::MappingValue> mappings_alloc =
                all_device_mappings.filter(Filter::is_map_alloc);

            struct aux
            {
                TL::ObjectList<OpenMP::MappingValue>& s;
                Nodecl::NodeclBase (*builder)(Nodecl::NodeclBase n);
            } nodes[4] = {
                { mappings_to,     make_map_node<Nodecl::OpenMP::MapTo> },
                { mappings_from,   make_map_node<Nodecl::OpenMP::MapFrom> },
                { mappings_tofrom, make_map_node<Nodecl::OpenMP::MapToFrom> },
                { mappings_alloc,  make_map_node<Nodecl::OpenMP::MapAlloc> }
            };

            Nodecl::List result;
            for (int i = 0; i < 4; i++)
            {
                if (nodes[i].s.empty())
                    continue;

                TL::ObjectList<OpenMP::MappingValue> &current_mappings = nodes[i].s;

                Nodecl::List current_list;
                for (TL::ObjectList<OpenMP::MappingValue>::iterator
                        it = current_mappings.begin();
                        it != current_mappings.end();
                        it++)
                {
                    ERROR_CONDITION(it->map_expr.is_null(), "Invalid tree at this point", 0);

                    current_list.append(it->map_expr.shallow_copy());
                }

                result.append(
                        nodes[i].builder(current_list)
                        );
            }

            return result;
        }
    }

    void Base::target_data_handler_pre(TL::PragmaCustomStatement ctr) { }
    void Base::target_data_handler_post(TL::PragmaCustomStatement ctr)
    {
        TL::PragmaCustomLine pragma_line = ctr.get_pragma_line();
        OpenMP::DataEnvironment &data_environment =
            _core.get_openmp_info()->get_data_environment(ctr);

        Nodecl::NodeclBase device_id, if_clause;
        handle_target_data_clauses(ctr,
                // out
                device_id,
                if_clause);

        if (this->emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << ctr.get_locus_str() << ": " << "TARGET DATA construct\n"
                << ctr.get_locus_str() << ": " << "---------------------\n"
                ;
            // TODO - Report explicit mappings
        }

        Nodecl::List device_data_environment;
        device_data_environment.append(device_id);
        if (!if_clause.is_null())
        {
            device_data_environment.append(if_clause);
        }

        Nodecl::NodeclBase map_clause = make_device_data_environment(data_environment);

        device_data_environment.append(map_clause);

        Nodecl::OpenMP::TargetData target_data =
            Nodecl::OpenMP::TargetData::make(
                    device_data_environment,
                    ctr.get_statements().shallow_copy(),
                    ctr.get_locus());

        pragma_line.diagnostic_unused_clauses();
        ctr.replace(target_data);
    }

    void Base::omp_target_handler_pre(TL::PragmaCustomStatement ctr) { }
    void Base::omp_target_handler_post(TL::PragmaCustomStatement ctr)
    {
        TL::PragmaCustomLine pragma_line = ctr.get_pragma_line();
        OpenMP::DataEnvironment &data_environment =
            _core.get_openmp_info()->get_data_environment(ctr);

        Nodecl::NodeclBase device_id, if_clause;
        handle_target_data_clauses(ctr,
                // out
                device_id,
                if_clause);

        if (this->emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << ctr.get_locus_str() << ": " << "TARGET construct\n"
                << ctr.get_locus_str() << ": " << "----------------\n"
                ;
            // TODO - Report explicit mappings
        }

        Nodecl::List device_data_environment;
        device_data_environment.append(device_id);
        if (!if_clause.is_null())
        {
            device_data_environment.append(if_clause);
        }

        TL::PragmaCustomClause nowait = pragma_line.get_clause("nowait");
        if (!nowait.is_defined())
        {
            device_data_environment.append(
                    Nodecl::OpenMP::TargetTaskUndeferred::make()
                    );
        }

        Nodecl::NodeclBase dependences = make_execution_environment(data_environment,
                pragma_line,
                /* ignore_target_info */ true);
        device_data_environment.append(dependences);

        Nodecl::NodeclBase map_clause = make_device_data_environment(data_environment);
        device_data_environment.append(map_clause);

        Nodecl::OpenMP::Target target_data =
            Nodecl::OpenMP::Target::make(
                    device_data_environment,
                    ctr.get_statements().shallow_copy(),
                    ctr.get_locus());

        pragma_line.diagnostic_unused_clauses();
        ctr.replace(target_data);
    }

    // Unused because they are not possible in OpenMP
    void Base::omp_target_handler_pre(TL::PragmaCustomDeclaration) { }
    void Base::omp_target_handler_post(TL::PragmaCustomDeclaration) { }

    namespace {
        template <typename T>
            Nodecl::NodeclBase make_motion_node(Nodecl::NodeclBase n)
            {
                return T::make(n, n.get_locus());
            }

        Nodecl::NodeclBase handle_motion_clauses(TL::PragmaCustomLine &pragma_line)
        {
            Nodecl::List result;

            TL::Scope parsing_scope = pragma_line.retrieve_context();
            TL::PragmaCustomClause to_clause = pragma_line.get_clause("to");
            TL::PragmaCustomClause from_clause = pragma_line.get_clause("from");

            struct aux
            {
                TL::PragmaCustomClause &clause;
                Nodecl::NodeclBase (*builder)(Nodecl::NodeclBase expr);
            } motions[2] = {
                { to_clause, make_motion_node<Nodecl::OpenMP::MotionTo> },
                { from_clause, make_motion_node<Nodecl::OpenMP::MotionFrom> },
            };

            for (int i = 0; i < 2; i++)
            {
                if (!motions[i].clause.is_defined())
                    continue;

                Nodecl::List expr_list;

                TL::ObjectList<std::string> args = motions[i].clause.get_tokenized_arguments();
                for (TL::ObjectList<std::string>::iterator it = args.begin();
                        it != args.end();
                        it++)
                {
                    Source src;
                    src << "#line " << pragma_line.get_line()
                        << " \"" << pragma_line.get_filename() << "\"\n";
                    src << pad_to_column(pragma_line.get_column()) << *it;

                    // Now, parse a single OpenMP list item
                    // Note that we reuse the same code that we use for items in
                    // dependences
                    Nodecl::NodeclBase expr;
                    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                    {
                        expr = src.parse_generic(parsing_scope,
                                /* ParseFlags */ Source::DEFAULT,
                                "@OMP-DEPEND-ITEM@",
                                Source::c_cxx_check_expression_adapter,
                                decl_context_map_id);
                    }
                    else if (IS_FORTRAN_LANGUAGE)
                    {
                        expr = src.parse_generic(parsing_scope,
                                /* ParseFlags */ Source::DEFAULT,
                                "@OMP-DEPEND-ITEM@",
                                Source::fortran_check_expression_adapter,
                                decl_context_map_id);
                    }
                    else
                    {
                        internal_error("Code unreachable", 0);
                    }

                    if (expr.is_null()
                            || nodecl_is_err_expr(expr.get_internal_nodecl()))
                        continue;

                    expr_list.append(expr);
                }

                if (!expr_list.empty())
                {
                    result.append(
                            motions[i].builder(expr_list));
                }
            }

            return result;
        }
    }

    void Base::target_update_handler_pre(TL::PragmaCustomDirective ctr) { }
    void Base::target_update_handler_post(TL::PragmaCustomDirective ctr)
    {
        TL::PragmaCustomLine pragma_line = ctr.get_pragma_line();

        Nodecl::NodeclBase device_id = handle_device_clause(pragma_line);

        Nodecl::List command_environment;
        command_environment.append(device_id);

        Nodecl::NodeclBase motion_commands = handle_motion_clauses(pragma_line);
        if (!motion_commands.is_null())
        {
            command_environment.append(motion_commands);
        }

        Nodecl::NodeclBase if_clause = handle_if_clause(pragma_line);
        if (!if_clause.is_null())
        {
            command_environment.append(if_clause);
        }

        Nodecl::OpenMP::TargetUpdate target_update =
            Nodecl::OpenMP::TargetUpdate::make(
                    command_environment,
                    ctr.get_locus());

        pragma_line.diagnostic_unused_clauses();
        ctr.replace(target_update);
    }
    
    namespace {

        Nodecl::NodeclBase handle_num_teams(TL::PragmaCustomLine pragma_line)
        {
            TL::PragmaCustomClause num_teams = pragma_line.get_clause("num_teams");

            if (!num_teams.is_defined())
                return Nodecl::NodeclBase::null();

            TL::ObjectList<Nodecl::NodeclBase> expr_list = num_teams.get_arguments_as_expressions();

            if (expr_list.empty())
            {
                error_printf_at(pragma_line.get_locus(), "empty 'num_teams' clause\n");
                return Nodecl::NodeclBase::null();
            }
            else
            {
                if (expr_list.size() > 1)
                {
                    error_printf_at(pragma_line.get_locus(), "too many expressions in 'num_teams' clause\n");
                }
                return Nodecl::OpenMP::NumTeams::make(
                        expr_list[0],
                        expr_list[0].get_locus());
            }
        }

        Nodecl::NodeclBase handle_thread_limit(TL::PragmaCustomLine pragma_line)
        {
            TL::PragmaCustomClause thread_limit = pragma_line.get_clause("thread_limit");

            if (!thread_limit.is_defined())
                return Nodecl::NodeclBase::null();

            TL::ObjectList<Nodecl::NodeclBase> expr_list = thread_limit.get_arguments_as_expressions();

            if (expr_list.empty())
            {
                error_printf_at(pragma_line.get_locus(), "empty 'thread_limit' clause\n");
                return Nodecl::NodeclBase::null();
            }
            else
            {
                if (expr_list.size() > 1)
                {
                    error_printf_at(pragma_line.get_locus(), "too many expressions in 'thread_limit' clause\n");
                }
                return Nodecl::OpenMP::ThreadLimit::make(
                        expr_list[0],
                        expr_list[0].get_locus());
            }
        }
    }

    void Base::teams_handler_pre(TL::PragmaCustomStatement ctr) { }
    void Base::teams_handler_post(TL::PragmaCustomStatement ctr)
    {
        OpenMP::DataEnvironment &data_environment =
            _core.get_openmp_info()->get_data_environment(ctr);

        TL::PragmaCustomLine pragma_line = ctr.get_pragma_line();

        if (this->emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << ctr.get_locus_str() << ": " << "TEAMS construct\n"
                << ctr.get_locus_str() << ": " << "---------------\n"
                ;
            // TODO
        }

        Nodecl::List execution_env;
        Nodecl::List data_sharings = make_execution_environment(
                data_environment,
                pragma_line,
                /* ignore_target_info */ true);

        execution_env.append(data_sharings);

        Nodecl::NodeclBase num_teams = handle_num_teams(pragma_line);
        if (!num_teams.is_null())
        {
            execution_env.append(num_teams);
        }

        Nodecl::NodeclBase thread_limit = handle_thread_limit(pragma_line);
        if (!thread_limit.is_null())
        {
            execution_env.append(thread_limit);
        }

        Nodecl::OpenMP::Teams teams = Nodecl::OpenMP::Teams::make(
                execution_env,
                ctr.get_statements(),
                ctr.get_locus());

        pragma_line.diagnostic_unused_clauses();
        ctr.replace(teams);
    }

    namespace {
        Nodecl::NodeclBase handle_dist_schedule(TL::PragmaCustomLine pragma_line)
        {
            Nodecl::NodeclBase result;

            TL::PragmaCustomClause dist_schedule = pragma_line.get_clause("dist_schedule");

            if (dist_schedule.is_defined())
            {
                ObjectList<std::string> arguments = dist_schedule.get_tokenized_arguments();

                if (arguments.empty())
                {
                    error_printf_at(pragma_line.get_locus(), "empty 'dist_schedule'\n");
                }
                else
                {
                    Nodecl::NodeclBase chunk;
                    std::string schedule = strtolower(arguments[0].c_str());

                    if (schedule != "static")
                    {
                        error_printf_at(pragma_line.get_locus(), "invalid schedule kind in 'dist_schedule', only 'static' is allowed\n");
                    }
                    else if (arguments.size() >= 2)
                    {
                        chunk = Source(arguments[1]).parse_expression(pragma_line);

                        if (arguments.size() > 2)
                        {
                            error_printf_at(pragma_line.get_locus(), "too many arguments in 'dist_schedule' clause\n");
                        }
                    }

                    if (chunk.is_null()
                            || nodecl_is_err_expr(chunk.get_internal_nodecl()))
                    {
                        chunk = const_value_to_nodecl(const_value_get_signed_int(0));
                    }

                    result = Nodecl::OpenMP::DistSchedule::make(
                            chunk,
                            schedule,
                            pragma_line.get_locus());
                }
            }

            if (result.is_null())
            {
                result = Nodecl::OpenMP::DistSchedule::make(
                        ::const_value_to_nodecl(const_value_get_signed_int(0)),
                        "static",
                        pragma_line.get_locus());
            }

            return result;
        }
    }

    void Base::distribute_handler_pre(TL::PragmaCustomStatement ctr) { }
    void Base::distribute_handler_post(TL::PragmaCustomStatement ctr)
    {
        Nodecl::NodeclBase statement = ctr.get_statements();
        ERROR_CONDITION(!statement.is<Nodecl::List>(), "Invalid tree", 0);
        statement = statement.as<Nodecl::List>().front();
        ERROR_CONDITION(!statement.is<Nodecl::Context>(), "Invalid tree", 0);

        OpenMP::DataEnvironment &data_environment =
            _core.get_openmp_info()->get_data_environment(ctr);

        TL::PragmaCustomLine pragma_line = ctr.get_pragma_line();

        if (this->emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << ctr.get_locus_str() << ": " << "DISTRIBUTE construct\n"
                << ctr.get_locus_str() << ": " << "--------------------\n"
                ;
            // TODO
        }

        Nodecl::List execution_env;
        Nodecl::List data_sharings = make_execution_environment(
                data_environment,
                pragma_line,
                /* ignore_target_info */ true);
        execution_env.append(data_sharings);

        Nodecl::NodeclBase dist_schedule = handle_dist_schedule(pragma_line);
        execution_env.append(dist_schedule);

        Nodecl::OpenMP::Distribute distribute = Nodecl::OpenMP::Distribute::make(
                execution_env,
                statement,
                ctr.get_locus());

        pragma_line.diagnostic_unused_clauses();
        ctr.replace(distribute);
    }

    void Base::declare_target_handler_pre(TL::PragmaCustomDirective ctr)
    {
        _start_declare_target = ctr;
    }
    void Base::declare_target_handler_post(TL::PragmaCustomDirective ctr) { }

    void Base::end_declare_target_handler_pre(TL::PragmaCustomDirective ctr) { }
    void Base::end_declare_target_handler_post(TL::PragmaCustomDirective ctr)
    {

        Nodecl::NodeclBase n = _start_declare_target.get_parent();
        ERROR_CONDITION(!n.is<Nodecl::List>(), "Invalid node", 0);

        // FIXME: this way of traversing a list bounded by two nodes can be
        // vastly improved
        // Skip the starting node
        n = n.get_parent();

        TL::ObjectList<TL::Symbol> symbol_set;

        for(;;)
        {
            // Something is amiss here
            if (n.is_null()
                    || !n.is<Nodecl::List>())
                break;

            Nodecl::NodeclBase::Children c = n.children();
            Nodecl::NodeclBase current = c[1];

            // We reached the end
            if (current == ctr)
                break;

            if (current.is<Nodecl::ObjectInit>())
            {
                info_printf_at(current.get_locus(), "target declaration of '%s'\n",
                        current.get_symbol().get_qualified_name().c_str());
                symbol_set.insert(current.get_symbol());
            }
            else if (current.is<Nodecl::FunctionCode>())
            {
                info_printf_at(current.get_locus(), "target declaration of function '%s'\n",
                        current.get_symbol().get_qualified_name().c_str());
                symbol_set.insert(current.get_symbol());
            }
            // else if (current.is<Nodecl::CxxDecl>())
            // {
            // }
            // else if (current.is<Nodecl::CxxDef>())
            // {
            // }
            else
            {
                // Ignore any other node for now
            }

            n = n.get_parent();
        }

        // Now remove start
        Nodecl::Utils::remove_from_enclosing_list(_start_declare_target);
        _start_declare_target = Nodecl::NodeclBase::null();

        struct MakeSymbol
        {
            static Nodecl::NodeclBase make(const TL::Symbol &sym, const locus_t* locus)
            {
                return sym.make_nodecl(locus);
            }
        };

        // And replace the current node
        TL::ObjectList<Nodecl::NodeclBase> nodecl_sym_list =
            symbol_set.map<Nodecl::NodeclBase>(std::bind(MakeSymbol::make, std::placeholders::_1, ctr.get_locus()));

        Nodecl::NodeclBase declare_target =
            Nodecl::OpenMP::DeclareTarget::make(
                    Nodecl::List::make(nodecl_sym_list),
                    ctr.get_locus());

        ctr.replace(declare_target);
    }

} }
