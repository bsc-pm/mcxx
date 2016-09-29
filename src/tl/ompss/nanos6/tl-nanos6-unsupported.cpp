/*--------------------------------------------------------------------
  (C) Copyright 2016-2016 Barcelona Supercomputing Center
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


#include "tl-nanos6-lower.hpp"
#include "cxx-diagnostic.h"

namespace TL
{
namespace Nanos6
{

namespace
{
void unsupported(const Nodecl::NodeclBase &n)
{
    error_printf_at(n.get_locus(),
                    "this OmpSs construct is not supported in Nanos6\n");
}
}


void Lower::visit(const Nodecl::OpenMP::Taskyield &n)
{
    unsupported(n);
}

void Lower::visit(const Nodecl::OpenMP::For &n)
{
    unsupported(n);
}

void Lower::visit(const Nodecl::OpenMP::BarrierFull &n)
{
    unsupported(n);
}

void Lower::visit(const Nodecl::OmpSs::WaitOnDependences &n)
{
    unsupported(n);
}

void Lower::visit(const Nodecl::OpenMP::Atomic &n)
{
    unsupported(n);
}

void Lower::visit(const Nodecl::OpenMP::FlushMemory &n)
{
    unsupported(n);
}
void Lower::visit(const Nodecl::OmpSs::Register &n)
{
    unsupported(n);
}

void Lower::visit(const Nodecl::OmpSs::Unregister &n)
{
    unsupported(n);
}

}
}
