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


#include "tl-omp-core.hpp"
#include "cxx-diagnostic.h"

// Needed for parsing OpenMP standard clauses
#include <sys/types.h>
#include <regex.h>

namespace TL { namespace OpenMP {

    namespace {
        const decl_context_t* decl_context_map_id(const decl_context_t* d)
        {
            return d;
        }
    }

    void Core::handle_map_clause(TL::PragmaCustomLine pragma_line,
            DataEnvironment& data_environment)
    {
        TL::Scope parsing_scope = pragma_line.retrieve_context();

        TL::ObjectList<Nodecl::NodeclBase> map_to;
        TL::ObjectList<Nodecl::NodeclBase> map_from;
        TL::ObjectList<Nodecl::NodeclBase> map_tofrom;

        TL::PragmaCustomClause map_clause = pragma_line.get_clause("map");
        if (!map_clause.is_defined())
            return;

        TL::ObjectList<std::string> arguments = map_clause.get_tokenized_arguments();
        if (arguments.empty())
        {
            error_printf("%s: error: empty 'map' clause\n",
                    pragma_line.get_locus_str().c_str());
            return;
        }

        // Since we coalesce all the arguments of a clauses with the same name
        // in a case like map(to : a, b) map(from : c, d) will be a list
        // containing "in:a", "b", "out:c", "d"

        int cflags = REG_EXTENDED;
        if (IS_FORTRAN_LANGUAGE)
        {
            cflags |= REG_ICASE;
        }

        regex_t preg;
        if (regcomp(&preg, "^[[:blank:]]*((to)|(from)|(tofrom))[[:blank:]]*:(.*)$", cflags) != 0)
        {
            internal_error("Invalid regular expression", 0);
        }
        const int num_matches = 6;
        regmatch_t pmatch[num_matches] = { };

        TL::ObjectList<Nodecl::NodeclBase> *map_set = NULL;
        for (ObjectList<std::string>::iterator it = arguments.begin();
                it != arguments.end();
                it++)
        {
            std::string clause_name;
            int match = regexec(&preg, it->c_str(), num_matches, pmatch, 0);

            std::string current_map_expr = *it;

            if (match == 0)
            {
                // Zero-th match is the whole regular expression
                ERROR_CONDITION(pmatch[1].rm_so == -1, "Invalid match", 0);
                std::string map_kind;
                for (int i = pmatch[1].rm_so; i < pmatch[1].rm_eo; i++)
                {
                    map_kind += tolower((*it)[i]);
                }

                if (map_kind == "to")
                {
                    map_set = &map_to;
                }
                else if (map_kind == "from")
                {
                    map_set = &map_from;
                }
                else if (map_kind == "tofrom")
                {
                    map_set = &map_tofrom;
                }
                else
                {
                    internal_error("Code unreachable", 0);
                }

                // Now compute the proper map expression
                current_map_expr.clear();
                ERROR_CONDITION(pmatch[5].rm_so == -1, "Invalid match", 0);
                for (int i = pmatch[5].rm_so; i < pmatch[5].rm_eo; i++)
                {
                    current_map_expr += (*it)[i];
                }
            }
            else if (match == REG_NOMATCH)
            {
                if (map_set == NULL)
                {
                    error_printf("%s: error: skipping item '%s' in 'map' clause because it lacks map-kind\n",
                            pragma_line.get_locus_str().c_str(),
                            current_map_expr.c_str());
                    continue;
                }
            }
            else
            {
                internal_error("Unexpected result %d from regexec\n", match);
            }

            Source src;
            src << "#line " << map_clause.get_pragma_line().get_line() << " \"" << map_clause.get_pragma_line().get_filename() << "\"\n";
            src << pad_to_column(map_clause.get_pragma_line().get_column()) << current_map_expr;

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

            map_set->append(expr);
        }

        regfree(&preg);
    }

    void Core::omp_target_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::omp_target_handler_post(TL::PragmaCustomStatement ctr) { }


    void Core::target_data_handler_pre(TL::PragmaCustomStatement ctr)
    {
        DataEnvironment& data_environment = _openmp_info->get_new_data_environment(ctr);
        _openmp_info->push_current_data_environment(data_environment);

        handle_map_clause(ctr.get_pragma_line(), data_environment);
    }

    void Core::target_data_handler_post(TL::PragmaCustomStatement ctr)
    {
        _openmp_info->pop_current_data_environment();
    }

    void Core::target_update_handler_pre(TL::PragmaCustomDirective ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::target_update_handler_post(TL::PragmaCustomDirective ctr) { }

    void Core::teams_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::teams_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::distribute_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::distribute_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::distribute_parallel_for_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::distribute_parallel_for_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::distribute_parallel_do_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::distribute_parallel_do_handler_post(TL::PragmaCustomStatement ctr) { }

    // Combined
    void Core::target_teams_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::target_teams_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::teams_distribute_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::teams_distribute_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::teams_distribute_parallel_for_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::teams_distribute_parallel_for_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::teams_distribute_parallel_do_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::teams_distribute_parallel_do_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::target_teams_distribute_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::target_teams_distribute_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::target_teams_distribute_parallel_for_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::target_teams_distribute_parallel_for_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::target_teams_distribute_parallel_do_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::target_teams_distribute_parallel_do_handler_post(TL::PragmaCustomStatement ctr) { }

} }
