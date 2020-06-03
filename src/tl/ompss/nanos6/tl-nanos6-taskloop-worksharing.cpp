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


#ifndef TL_NANOS6_TASKLOOP_WORKSHARING_HPP
#define TL_NANOS6_TASKLOOP_WORKSHARING_HPP

#include "tl-nanos6-lower.hpp"
#include "tl-nanos6-interface.hpp"

namespace TL { namespace Nanos6 {

    void Lower::visit(const Nodecl::OmpSs::TaskloopWorksharing& construct)
    {
        Interface::family_must_be_at_least("nanos6_instantiation_api", 2, "to support taskloop");
        Interface::family_must_be_at_least("nanos6_loop_api", 2, "to support taskloop");

        Nodecl::NodeclBase loop = construct.get_loop();

        walk(loop);

        Nodecl::List exec_env = construct.get_environment().as<Nodecl::List>();
        // Set taskloop as worksharing
        exec_env.append(Nodecl::OmpSs::TaskIsLoop::make());

        construct.replace(
                Nodecl::OpenMP::Taskloop::make(exec_env, loop, construct.get_locus()));

        Nodecl::NodeclBase serial_stmts;
        // If disabled, act normally
        if (!_phase->_final_clause_transformation_disabled)
        {
            std::map<Nodecl::NodeclBase, Nodecl::NodeclBase>::iterator it = _final_stmts_map.find(construct);
            ERROR_CONDITION(it == _final_stmts_map.end(), "Invalid serial statemtents", 0);
            serial_stmts = it->second;
        }

        lower_taskloop(construct.as<Nodecl::OpenMP::Taskloop>(), serial_stmts);
    }

}}

#endif // TL_NANOS6_TASKLOOP_WORKSHARING_HPP
