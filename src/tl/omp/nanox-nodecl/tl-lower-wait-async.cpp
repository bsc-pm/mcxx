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

#include "tl-lowering-visitor.hpp"
#include "tl-counters.hpp"
#include "tl-source.hpp"

namespace TL { namespace Nanox {

void LoweringVisitor::visit(const Nodecl::Parallel::WaitAsyncsShallow& construct)
{
    Counter& current_wd = CounterManager::get_counter("nanos++-taskwait");
    Counter& err_counter = CounterManager::get_counter("nanos++-err");

    Source src, wd_name, err_name;

    err_name << "err";

    src << "{"
        <<     "nanos_wd_t " << wd_name << ";"
        <<     wd_name << " = nanos_current_wd();"
        <<     "nanos_err_t " << err_name << ";"
        <<     err_name << "= nanos_wg_wait_completion(" << wd_name << ", 0);"
        << "}"
        ;

    wd_name << "current_wd_" << (int)current_wd;
    current_wd++;

    FORTRAN_LANGUAGE()
    {
        // Parse in C
        Source::source_language = SourceLanguage::C;
    }

    Nodecl::NodeclBase n = src.parse_statement(construct);

    FORTRAN_LANGUAGE()
    {
        Source::source_language = SourceLanguage::Current;
    }

    construct.integrate(n);
}

} }
