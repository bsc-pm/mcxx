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

    void LoweringVisitor::visit(const Nodecl::OpenMP::For& construct)
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

        if (is_ompss_schedule)
           lower_for_slicer(construct);
        else
            lower_for_worksharing(construct);
    }

    Source LoweringVisitor::update_lastprivates(OutlineInfo& outline_info, const std::string &loop_descriptor_name)
    {
        Source lastprivate_updates;

        TL::ObjectList<OutlineDataItem*> outline_data_items = outline_info.get_data_items();

        int num_items = 0;
        for (TL::ObjectList<OutlineDataItem*>::iterator it = outline_data_items.begin();
                it != outline_data_items.end();
                it++)
        {
            if ((*it)->get_is_lastprivate())
            {
                ERROR_CONDITION((*it)->get_lastprivate_shared() == NULL, "This cannot be NULL", 0);
                if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                {
                    if ((*it)->get_private_type().is_array())
                    {
                        lastprivate_updates
                            << "__builtin_memcpy(" << (*it)->get_lastprivate_shared()->get_symbol().get_name()
                            << "," << (*it)->get_symbol().get_name() << ", "
                            << "sizeof(" << as_type((*it)->get_private_type()) << "));"
                            ;
                    }
                    else
                    {
                        lastprivate_updates
                            << "*" << (*it)->get_lastprivate_shared()->get_symbol().get_name()
                            << " = " << (*it)->get_symbol().get_name() << ";"
                            ;
                    }
                }
                else if (IS_FORTRAN_LANGUAGE)
                {
                    lastprivate_updates
                        << (*it)->get_lastprivate_shared()->get_symbol().get_name()
                        << " = " << (*it)->get_symbol().get_name() << "\n"
                        ;
                }
                num_items++;
            }
        }

        Source lastprivate_code;

        if (num_items > 0)
        {
            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
            lastprivate_code
                << "if (" << loop_descriptor_name << ".last)"
                << "{"
                <<     lastprivate_updates
                << "}"
                ;
            }
            else
            {
                lastprivate_code
                    << "IF (" << loop_descriptor_name << "% last) THEN\n"
                    <<     lastprivate_updates
                    << "END IF\n"
                    ;
            }
        }

        return lastprivate_code;
    }

} }
