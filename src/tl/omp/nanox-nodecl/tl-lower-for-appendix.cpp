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


#include "tl-source.hpp"
#include "tl-lowering-visitor.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-counters.hpp"
#include "cxx-cexpr.h"
#include "tl-predicateutils.hpp"
#include "tl-devices.hpp"
#include "tl-nanos.hpp"

namespace TL { namespace Nanox {

    void LoweringVisitor::visit(const Nodecl::OpenMP::ForAppendix& construct)
    {
        Nodecl::List distribute_environment = construct.get_environment().as<Nodecl::List>();
        Nodecl::OpenMP::Schedule schedule = distribute_environment.find_first<Nodecl::OpenMP::Schedule>();
        ERROR_CONDITION(schedule.is_null(), "Schedule tree is missing", 0);

        std::string schedule_name = schedule.get_text();

        std::string ompss_prefix = "ompss_";
        bool is_ompss_schedule = (schedule_name.substr(0, ompss_prefix.size()) == ompss_prefix);

        std::string openmp_prefix0 = "omp_";
        std::string openmp_prefix1 = "openmp_";
        bool is_explicit_openmp_schedule = (schedule_name.substr(0, openmp_prefix0.size()) == openmp_prefix0)
            || (schedule_name.substr(0, openmp_prefix1.size()) == openmp_prefix1);

        if (!is_ompss_schedule
                && !is_explicit_openmp_schedule)
        {
            // If the user just requested 'sched' and we are in OmpSs use 'ompss_sched'
            if (_lowering->in_ompss_mode())
            {
                std::string fixed_schedule = schedule_name;
                fixed_schedule = "ompss_" + schedule_name;
                is_ompss_schedule = true;
                schedule.set_text(fixed_schedule);
            }
        }

        if (is_explicit_openmp_schedule)
        {
            // If the user requested 'omp_sched' or 'openmp_sched' use 'sched'
            std::string fixed_schedule;
            if (schedule_name.substr(0, openmp_prefix0.size()) == openmp_prefix0)
                fixed_schedule = schedule_name.substr(openmp_prefix0.size());
            else if (schedule_name.substr(0, openmp_prefix1.size()) == openmp_prefix1)
                fixed_schedule = schedule_name.substr(openmp_prefix1.size());
            schedule.set_text(fixed_schedule);
        }

        // FIXME - Implement final closure for ForAppendix
        if (is_ompss_schedule)
        {
           Nodecl::NodeclBase new_construct = construct;

#if 0
           bool generate_final_stmts =
              Nanos::Version::interface_is_at_least("master", 5024) &&
              !_lowering->final_clause_transformation_disabled();

           // If the current programming model is OmpSs and the support to the
           // final clause is not disabled
           if (_lowering->in_ompss_mode() && generate_final_stmts)
           {
              // We create a new Node OpenMP::For with the same childs as the
              // original construct. Another solution is shallow copy all the
              // construct (less efficient)
              new_construct =
                 Nodecl::OpenMP::ForAppendix::make(distribute_environment,
                         construct.get_loop(),
                         construct.get_prependix(),
                         construct.get_appendix());

              Nodecl::NodeclBase copied_statements_placeholder;
              TL::Source code;
              code
                 << "{"
                 <<      as_type(TL::Type::get_bool_type()) << "mcc_is_in_final;"
                 <<      "nanos_err_t mcc_err_in_final = nanos_in_final(&mcc_is_in_final);"
                 <<      "if (mcc_err_in_final != NANOS_OK) nanos_handle_error(mcc_err_in_final);"
                 <<      "if (mcc_is_in_final)"
                 <<      "{"
                 <<          statement_placeholder(copied_statements_placeholder)
                 <<      "}"
                 <<      "else"
                 <<      "{"
                 <<          as_statement(new_construct)
                 <<      "}"
                 << "}"
                 ;

              if (IS_FORTRAN_LANGUAGE)
                 Source::source_language = SourceLanguage::C;

              Nodecl::NodeclBase if_else_tree = code.parse_statement(construct);

              if (IS_FORTRAN_LANGUAGE)
                 Source::source_language = SourceLanguage::Current;

              construct.replace(if_else_tree);

              // We obtain the list node which contains the placeholder used to store
              // the final stmts. This must be done before the replace because at
              // this point the parent of the copied_statements_placeholder is the
              // first (and the unique) list node
              Nodecl::NodeclBase final_stmt_list = copied_statements_placeholder.get_parent();

              std::map<Nodecl::NodeclBase, Nodecl::NodeclBase>::iterator it = _final_stmts_map.find(construct);

              ERROR_CONDITION(it == _final_stmts_map.end(), "Unreachable code", 0);

              // We need to replace the placeholder before transforming the OpenMP/OmpSs pragmas
              copied_statements_placeholder.replace(it->second);

              ERROR_CONDITION(!copied_statements_placeholder.is_in_list(), "Unreachable code\n", 0);

              // Walk over the tree transforming OpenMP/OmpSs non-task pragmas
              walk(final_stmt_list);
           }
#endif
           lower_for_slicer(new_construct.as<Nodecl::OpenMP::For>(),
                   construct.get_prependix(),
                   construct.get_appendix());
        }
        else
        {
            lower_for_worksharing(construct.as<Nodecl::OpenMP::For>(),
                    construct.get_prependix(),
                    construct.get_appendix());
        }
    }

} }

