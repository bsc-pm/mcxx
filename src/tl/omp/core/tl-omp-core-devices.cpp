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
        TL::ObjectList<Nodecl::NodeclBase> map_alloc;

        TL::PragmaCustomClause map_clause = pragma_line.get_clause("map");
        if (!map_clause.is_defined())
            return;

        TL::ObjectList<std::string> arguments = map_clause.get_tokenized_arguments();
        if (arguments.empty())
        {
            error_printf_at(pragma_line.get_locus(),
                    "empty 'map' clause\n");
            return;
        }

        // Since we coalesce all the arguments of a clauses with the same name
        // in a case like map(to : a, b) map(from : c, d) will be a list
        // containing "to:a", "b", "from:c", "d"

        int cflags = REG_EXTENDED;
        if (IS_FORTRAN_LANGUAGE)
        {
            cflags |= REG_ICASE;
        }

        regex_t preg;
        if (regcomp(&preg, "^[[:blank:]]*((to)|(from)|(tofrom)|(alloc))[[:blank:]]*:(.*)$", cflags) != 0)
        {
            internal_error("Invalid regular expression", 0);
        }
        const int num_matches = 7;
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
                else if (map_kind == "alloc")
                {
                    map_set = &map_alloc;
                }
                else
                {
                    internal_error("Code unreachable", 0);
                }

                // Now compute the proper map expression
                current_map_expr.clear();
                ERROR_CONDITION(pmatch[6].rm_so == -1, "Invalid match", 0);
                for (int i = pmatch[6].rm_so; i < pmatch[6].rm_eo; i++)
                {
                    current_map_expr += (*it)[i];
                }
            }
            else if (match == REG_NOMATCH)
            {
                if (map_set == NULL)
                {
                    error_printf_at(
                            pragma_line.get_locus(),
                            "skipping item '%s' in 'map' clause because it lacks map-kind\n",
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

            if (expr.is_null()
                    || nodecl_is_err_expr(expr.get_internal_nodecl()))
                continue;

            map_set->append(expr);
        }

        regfree(&preg);

        // TL::ObjectList<Nodecl::NodeclBase> map_to;
        // TL::ObjectList<Nodecl::NodeclBase> map_from;
        // TL::ObjectList<Nodecl::NodeclBase> map_tofrom;
        struct aux {
            TL::ObjectList<Nodecl::NodeclBase> &set;
            MapDirection direction;
        } sets[3] = {
            { map_to, OpenMP::MAP_DIR_TO },
            { map_from, OpenMP::MAP_DIR_FROM },
            { map_tofrom, OpenMP::MAP_DIR_TOFROM },
        };

        for (int i = 0; i < 3; i++)
        {
            for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = sets[i].set.begin();
                    it != sets[i].set.end();
                    it++)
            {
                DataReference data_ref(*it);
                if (data_ref.is_valid())
                {
                    MappingValue map_value(sets[i].direction, MAP_KIND_EXPLICIT, *it);
                    data_environment.set_device_mapping(
                            data_ref.get_base_symbol(),
                            map_value,
                            "explicitly specified in map clause");
                }
                else
                {
                    error_printf_at(it->get_locus(),
                            "invalid expression '%s' in 'map' clause\n",
                            it->prettyprint().c_str());
                }
            }
        }
    }

    void Core::compute_implicit_device_mappings(
            Nodecl::NodeclBase stmt,
            DataEnvironment& data_environment,
            DefaultMapValue defaultmap_value)
    {
        ObjectList<TL::Symbol> nonlocal_symbols =
            Nodecl::Utils::get_nonlocal_symbols_first_occurrence(stmt)
            .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol);

        for (TL::ObjectList<TL::Symbol>::iterator it = nonlocal_symbols.begin();
                it != nonlocal_symbols.end();
                it++)
        {
            Symbol sym = *it;

            if (!sym.is_valid()
                    || !sym.is_variable()
                    || sym.is_fortran_parameter())
                continue;

            if (sym.is_member()
                    && !sym.is_static())
                continue;

            // Not sure how to map this
            if (IS_CXX_LANGUAGE
                    && sym.get_name() == "this")
                continue;

            // We cannot map these
            if (sym.is_thread()
                    || sym.is_thread_local())
                continue;

            DataSharingValue data_sharing = data_environment.get_data_sharing(sym);
            // We cannot map these either
            if ((data_sharing.attr & DS_THREADPRIVATE) == DS_THREADPRIVATE)
                continue;

            if (data_environment.get_device_mapping(sym).direction == MAP_DIR_UNDEFINED)
            {
                MappingValue map_value(MAP_DIR_TOFROM, MAP_KIND_IMPLICIT,
                        sym.make_nodecl(/* set_ref_type */ true, sym.get_locus()));
                data_environment.set_device_mapping(
                        sym,
                        map_value,
                        "implicitly mapped tofrom because "
                        "is used inside the data device environment");
            }
        }

        // FIXME - There are more cases that cause variables be implicitly mapped (e.g. VLAs)
    }

    void Core::omp_target_handler_pre(TL::PragmaCustomStatement ctr)
    {
        DataEnvironment& data_environment = _openmp_info->get_new_data_environment(ctr);
        _openmp_info->push_current_data_environment(data_environment);

        TL::PragmaCustomLine pragma_line = ctr.get_pragma_line();

        // map
        handle_map_clause(pragma_line, data_environment);

        TL::ObjectList<Symbol> extra_symbols;
        // private | firstprivate
        get_data_explicit_attributes(
                pragma_line,
                ctr.get_statements(),
                data_environment,
                extra_symbols);
        get_data_implicit_attributes(ctr,
                DS_SHARED,
                data_environment,
                /* there_is_default_clause */ false);

        PragmaCustomClause depend_clause = pragma_line.get_clause("depend");
        // depend
        get_dependences_openmp(
                depend_clause,
                pragma_line,
                data_environment,
                /* default-data-sharing */ DS_NONE,
                extra_symbols);
        get_data_extra_symbols(data_environment, extra_symbols);

        DefaultMapValue defaultmap_value = DEFAULTMAP_NONE;
        TL::PragmaCustomClause defaultmap_clause = pragma_line.get_clause("defaultmap");
        if (defaultmap_clause.is_defined())
        {
            TL::ObjectList<std::string> defaultmap_args = defaultmap_clause.get_tokenized_arguments();
            if (defaultmap_args.size() == 1
                    && defaultmap_args[0] == "tofrom:scalar")
            {
                defaultmap_value = DEFAULTMAP_SCALAR;
            }
            else
            {
                error_printf_at(
                        ctr.get_locus(),
                        "invalid argument '%s' for clause 'defaultmap', it should be 'tofrom:scalar'\n",
                        concat_strings(defaultmap_clause.get_raw_arguments()).c_str());
            }
        }

        compute_implicit_device_mappings(ctr.get_statements(), data_environment, defaultmap_value);
    }

    void Core::omp_target_handler_post(TL::PragmaCustomStatement ctr)
    {
        _openmp_info->pop_current_data_environment();
    }

    void Core::target_data_handler_pre(TL::PragmaCustomStatement ctr)
    {
        DataEnvironment& data_environment = _openmp_info->get_new_data_environment(ctr);
        _openmp_info->push_current_data_environment(data_environment);

        handle_map_clause(ctr.get_pragma_line(), data_environment);
        // FIXME: We should not be doing this
        // compute_implicit_device_mappings(ctr.get_statements(), data_environment);
    }

    void Core::target_data_handler_post(TL::PragmaCustomStatement ctr)
    {
        _openmp_info->pop_current_data_environment();
    }

    void Core::target_update_handler_pre(TL::PragmaCustomDirective ctr)
    {
        DataEnvironment& data_environment = _openmp_info->get_new_data_environment(ctr);
        _openmp_info->push_current_data_environment(data_environment);
    }

    void Core::target_update_handler_post(TL::PragmaCustomDirective ctr)
    {
        _openmp_info->pop_current_data_environment();
    }

    void Core::common_teams_handler(
            TL::PragmaCustomStatement construct,
            DataEnvironment& data_environment,
            ObjectList<Symbol>& extra_symbols)
    {
        data_environment.set_is_teams(true);

        common_construct_handler(construct, data_environment, extra_symbols);
    }

    void Core::teams_handler_pre(TL::PragmaCustomStatement ctr)
    {
        DataEnvironment& data_environment = _openmp_info->get_new_data_environment(ctr);
        _openmp_info->push_current_data_environment(data_environment);

        TL::ObjectList<Symbol> extra_symbols;
        common_teams_handler(ctr, data_environment, extra_symbols);
        get_data_extra_symbols(data_environment, extra_symbols);
    }

    void Core::teams_handler_post(TL::PragmaCustomStatement ctr)
    {
        _openmp_info->pop_current_data_environment();
    }

    void Core::distribute_handler_pre(TL::PragmaCustomStatement ctr)
    {
        Nodecl::NodeclBase loop = get_statement_from_pragma(ctr);
        loop_handler_pre(ctr, loop, &Core::common_for_handler);
    }

    void Core::distribute_handler_post(TL::PragmaCustomStatement ctr)
    {
        _openmp_info->pop_current_data_environment();
    }

    void Core::distribute_parallel_for_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf_at(ctr.get_locus(), "OpenMP 4.0 construct not implemented yet\n");
    }
    void Core::distribute_parallel_for_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::distribute_parallel_do_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf_at(ctr.get_locus(), "OpenMP 4.0 construct not implemented yet\n");
    }
    void Core::distribute_parallel_do_handler_post(TL::PragmaCustomStatement ctr) { }

    // Combined
    void Core::target_teams_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf_at(ctr.get_locus(), "OpenMP 4.0 construct not implemented yet\n");
    }
    void Core::target_teams_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::teams_distribute_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf_at(ctr.get_locus(), "OpenMP 4.0 construct not implemented yet\n");
    }
    void Core::teams_distribute_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::teams_distribute_parallel_for_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf_at(ctr.get_locus(), "OpenMP 4.0 construct not implemented yet\n");
    }
    void Core::teams_distribute_parallel_for_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::teams_distribute_parallel_do_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf_at(ctr.get_locus(), "OpenMP 4.0 construct not implemented yet\n");
    }
    void Core::teams_distribute_parallel_do_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::target_teams_distribute_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf_at(ctr.get_locus(), "OpenMP 4.0 construct not implemented yet\n");
    }
    void Core::target_teams_distribute_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::target_teams_distribute_parallel_for_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf_at(ctr.get_locus(), "OpenMP 4.0 construct not implemented yet\n");
    }
    void Core::target_teams_distribute_parallel_for_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::target_teams_distribute_parallel_do_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf_at(ctr.get_locus(), "OpenMP 4.0 construct not implemented yet\n");
    }
    void Core::target_teams_distribute_parallel_do_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::declare_target_handler_pre(TL::PragmaCustomDirective ctr)
    {
        if (_inside_declare_target)
        {
            error_printf_at(ctr.get_locus(), "nesting of '#pragma omp declare target' not implemented\n");
        }
        _inside_declare_target = true;
    }
    void Core::declare_target_handler_post(TL::PragmaCustomDirective ctr) { }

    void Core::end_declare_target_handler_pre(TL::PragmaCustomDirective ctr) { }
    void Core::end_declare_target_handler_post(TL::PragmaCustomDirective ctr)
    {
        if (!_inside_declare_target)
        {
            error_printf_at(ctr.get_locus(), "invalid nesting of '#pragma omp end declare target'\n");
        }
        _inside_declare_target = false;
    }

} }
