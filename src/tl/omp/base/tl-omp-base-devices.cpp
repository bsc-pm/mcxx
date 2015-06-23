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
                    error_printf("%s: error: empty 'device' clause\n", locus_to_str(locus));
                }
            }
            else 
            {
                device_id_expr = expr_list[0];
                if (expr_list.size() > 1)
                {
                    error_printf("%s: error: too many expressions in 'device' clause\n",
                            expr_list[1].get_locus_str().c_str());
                }
            }
            ERROR_CONDITION(device_id_expr.is_null(), "Expecting a valid device id here", 0);
            
            if (!::is_integer_type(no_ref(device_id_expr.get_type().get_internal_type())))
            {
                error_printf("%s: error: expression of 'device' must be an integer-expression\n",
                        device_id_expr.get_locus_str().c_str());
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
                error_printf("%s: error: expecting expression in 'if' clause\n",
                        locus_to_str(locus));
            }
            else
            {
                if (expr_list.size() > 1)
                {
                    error_printf("%s: error: too many expressions in 'if' clause\n",
                            expr_list[1].get_locus_str().c_str());
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

        ctr.replace(target_data);
    }

    void Base::omp_target_handler_pre(TL::PragmaCustomStatement ctr) { }
    void Base::omp_target_handler_post(TL::PragmaCustomStatement ctr)
    {
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

        Nodecl::NodeclBase map_clause = make_device_data_environment(data_environment);

        device_data_environment.append(map_clause);

        Nodecl::OpenMP::Target target_data =
            Nodecl::OpenMP::Target::make(
                    device_data_environment,
                    ctr.get_statements().shallow_copy(),
                    ctr.get_locus());

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
                error_printf("%s: error: empty 'num_teams' clause\n",
                        pragma_line.get_locus_str().c_str());
                return Nodecl::NodeclBase::null();
            }
            else
            {
                if (expr_list.size() > 1)
                {
                    error_printf("%s: error: too many expressions in 'num_teams' clause\n",
                            pragma_line.get_locus_str().c_str());
                }
                return Nodecl::OpenMP::NumTeams::make(
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
                /* ignore_target_info */ true,
                /* is_inline_task */ true);

        execution_env.append(data_sharings);

        Nodecl::NodeclBase num_teams = handle_num_teams(pragma_line);
        if (!num_teams.is_null())
        {
            execution_env.append(num_teams);
        }

        Nodecl::OpenMP::Teams teams = Nodecl::OpenMP::Teams::make(
                execution_env,
                ctr.get_statements(),
                ctr.get_locus());
    }

} }
