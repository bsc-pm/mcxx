/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

namespace TL { namespace Nanox {

    void LoweringVisitor::visit(const Nodecl::OpenMP::For& construct)
    {
        Nodecl::List distribute_environment = construct.get_environment().as<Nodecl::List>();
        Nodecl::OpenMP::Schedule schedule = distribute_environment.find_first<Nodecl::OpenMP::Schedule>();
        ERROR_CONDITION(schedule.is_null(), "Schedule tree is missing", 0);

        std::string schedule_name = schedule.get_text();

        std::string ompss_prefix = "ompss_";
        bool is_ompss_schedule = (schedule_name.substr(0, ompss_prefix.size()) == ompss_prefix);

        if (_lowering->in_ompss_mode() != is_ompss_schedule)
        {
            if (_lowering->in_ompss_mode())
            {
                std::string fixed_schedule = "ompss_" + schedule_name;
                schedule.set_text(fixed_schedule);

                std::cerr
                    << construct.get_locus()
                    << ": warning: schedule(" << schedule_name
                    << ") is not allowed in OmpSs mode. Assuming schedule(" << fixed_schedule << ")" << std::endl
                    ;
            }
            else
            {
                std::string fixed_schedule = schedule_name.substr(ompss_prefix.size());
                schedule.set_text(fixed_schedule);

                std::cerr
                    << construct.get_locus()
                    << ": warning: schedule(" << schedule_name
                    << ") is not allowed in OpenMP mode. Assuming schedule(" << fixed_schedule << ")" << std::endl
                    ;
            }
        }

        if (_lowering->in_ompss_mode())
        {
            lower_for_slicer(construct);
        }
        else
        {
            lower_for_worksharing(construct);
        }
    }

    Source LoweringVisitor::update_lastprivates(OutlineInfo& outline_info)
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
                if ((IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                        && (*it)->get_private_type().is_array())
                {
                    lastprivate_updates
                        << "__builtin_memcpy(" << (*it)->get_symbol().get_name() << "_addr, " 
                        << (*it)->get_symbol().get_name() << ", "
                        << "sizeof(" << as_type((*it)->get_private_type()) << "));"
                        ;
                }
                else
                {
                    lastprivate_updates
                        << "*(" << (*it)->get_symbol().get_name() << "_addr) = " << (*it)->get_symbol().get_name() << ";"
                        ;
                }
                num_items++;
            }
        }

        Source lastprivate_code;

        if (num_items > 0)
        {
            lastprivate_code
                << "if (nanos_item_loop.last)"
                << "{"
                <<     lastprivate_updates
                << "}"
                ;
        }

        return lastprivate_code;
    }

} }
