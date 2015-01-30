/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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

namespace TL { namespace Nanox {
    Source LoweringVisitor::full_barrier_source()
    {
        Source barrier_src;

        barrier_src
            << "{"
            << "nanos_err_t nanos_err;"
            << "nanos_err = nanos_omp_barrier();"
            << "if (nanos_err != NANOS_OK)"
            <<    "nanos_handle_error(nanos_err);"
            << "}"
            ;

        return barrier_src;
    }

    void LoweringVisitor::visit(const Nodecl::OpenMP::BarrierFull& construct)
    {
        Source barrier_src = full_barrier_source();

        FORTRAN_LANGUAGE()
        {
            Source::source_language = SourceLanguage::C;
        }
        Nodecl::NodeclBase barrier = barrier_src.parse_statement(construct);
        FORTRAN_LANGUAGE()
        {
            Source::source_language = SourceLanguage::Current;
        }

        construct.replace(barrier);
    }
} }
